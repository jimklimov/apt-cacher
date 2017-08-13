#! /usr/bin/perl
#
# lib/apt-cacher.pl
#
# This is a library file for apt-cacher to allow code common to apt-cacher
# itself plus its supporting scripts to be maintained in one location.

use strict;
use warnings;
use POSIX ();
use Fcntl qw/O_CREAT O_WRONLY :flock/;
use Storable ();
use Socket ();
use HTTP::Response;
use Dpkg::Arch;
use URI;
use IO::Uncompress::AnyUncompress qw($AnyUncompressError);
use Module::Load::Conditional;
use File::Spec;
use File::Path ();
use Carp;
our $cfg;

umask 0022;

sub read_config {

    (my $config_file) = @_;

    # set the default config variables
    my %config = (
		  # General
		  admin_email => 'root@localhost',
		  allowed_hosts => '',
		  allowed_ssl_locations => '',
		  allowed_ssl_ports => '443',
		  cache_dir => '/var/cache/apt-cacher',
		  clean_cache => 1,
		  concurrent_import_limit => eval {my $count = 0;
						   if (open(my $fh, '<', '/proc/cpuinfo')){
						       /^processor\s*:/i && $count++ foreach <$fh>;
						   }
						   $count},
		  curl_idle_timeout => 120,
		  curl_throttle => 10,
		  daemon_port => 3142,
		  debug => 0,
		  denied_hosts => '',
		  distinct_namespaces => 0,
		  expire_hours => 0,
		  data_timeout => 120,
		  generate_reports => 1,
		  group => eval {my $g = $); $g =~ s/\s.*$//; $g},
		  http_proxy => '',
		  libcurl_socket => '/var/run/apt-cacher/libcurl.socket',
		  limit => 0,
		  limit_global => 0,
		  log_dir => '/var/log/apt-cacher',
		  request_empty_lines => 5,
		  request_timeout => 10,
		  return_buffer_size => 1048576, # 1Mb
		  reverse_path_map => 1,
		  supported_archs => join(', ', qw(
						      avr32
						      amd64
						      alpha
						      arm
						      arm64
						      armel
						      armhf
						      hppa
						      hurd-i386
						      i386
						      ia64
						      kfreebsd-amd64
						      kfreebsd-i386
						      m32r
						      m68k
						      mips
						      mipsel
						      netbsd-alpha
						      netbsd-i386
						      powerpc
						      powerpcspe
						      ppc64
						      s390
						      s390x
						      sh4
						      sparc
						      sparc64
						      x32
						 )),
		  ubuntu_release_names => join(', ', qw(
							   dapper
							   edgy
							   feisty
							   gutsy
							   hardy
							   intrepid
							   jaunty
							   karmic
							   lucid
							   maverick
							   natty
							   oneiric
							   precise
							   quantal
							   raring
							   saucy
							   trusty
							   utopic
							   vivid
							   wily
							   xenial
							   yakkety
							   zesty
						      )),
		  user => $>,

		  # Private
		  _config_file => $config_file,
		  _path_map => {
				'debian-changelogs' => ['packages.debian.org', 'metadata.ftp-master.debian.org'],
				'debian-appstream' => ['appstream.debian.org'],
				'ubuntu-changelogs' => ['changelogs.ubuntu.com'],
				'ubuntu-appstream' => ['appstream.ubuntu.com']
			       },

		  # Regexps
		  checksum_files_regexp => '(?:^|/)(?:' . join('|',
							 qw((?:Sources|Packages)(?:\.(?:x|g)z|\.bz2)?
							    (?:In)?Release
							    Index(?:\.bz2)?
							  )
							      ) . ')$',
		  skip_checksum_files_regexp => '(?:^|/)(?:' . join('|',
							qw((?:In)?Release
							   Release\.gpg
							 )
								   ) . ')$',
		  index_files_regexp => '(?:^|/)(?:' . join('|',
						      qw(Index(?:\.bz2)?
							 (?:Sources|Packages|release)(?:\.(?:x|g)z|\.bz2)?
							 Release(?:\.gpg)?
							 InRelease
							 Contents-(?:[a-z]+-)?[a-zA-Z0-9]+\.gz
							 (?:srclist|pkglist)\.[a-z-]+\.bz2
							 Components-%VALID_ARCHS%\.yml\.(?:x|g)z
							 icons-(64|128)x\g{-1}\.tar\.(?:x|g)z
						       ),
						      # This needs to be a separate item to avoid a warning from the
						      # comma within qw()
						      q(Translation-[a-z]{2,3}(?:_[A-Z]{2}(?:\.[a-zA-Z0-9-]+)?)?(?:\.gz|\.bz2|\.xz|\.lzma)?)
							   ) . ')$',
		  installer_files_regexp => '(?:^|/)(?:' . join('|',
							  qw(vmlinuz
							     linux
							     initrd\.gz
							     (?:%VALID_PACKAGE_NAME%_%VALID_VERSION%[_\.])?changelog
							     NEWS\.Debian
							     %VALID_UBUNTU_RELEASE_NAMES%\.tar\.gz(?:\.gpg)?
							     (?:by-hash/(?i:MD5SUM/[0-9a-f]{32}|SHA1/[0-9a-f]{40}|SHA256/[0-9a-f]{64}))
							     (?:Devel|EOL)?ReleaseAnnouncement(?:\.html)?
							     meta-release(?:-lts)?(?:-(?:development|proposed))?
							   )
							       ) . ')$',
		  package_files_regexp => '(?:' . join('|',
						       qw((?:^|/)%VALID_PACKAGE_NAME%_%VALID_VERSION%(?:_%VALID_ARCHS%\.(?:u|d)?deb|\.dsc|\.tar\.(?:gz|bz2|xz|lzma)(?:\.asc)?|\.diff\.gz)
							  \.rpm
							  index\.db-.+\.gz
							  \.jigdo
							  \.template
							)
						      ) .')$',
		  pdiff_files_regexp => '(?:^|/)2\d{3}-\d{2}-\d{2}-\d{4}\.\d{2}\.gz$',
		  soap_url_regexp => '^(?:http://)?bugs\.debian\.org(?::80)?/cgi-bin/soap\.cgi$',
		 );

  CONFIGFILE:
    foreach my $file ($config_file, grep {!/(?:\.(?:disabled|dpkg-(?:old|dist|new|tmp))|~)$/} glob((File::Spec->splitpath($config_file))[1].'conf.d/*')) {

	open my $fh, '<', $file or die $!;
	local $/; # Slurp
	if (my $buf = $fh->getline) {
	    $buf=~s/\\\n#/\n#/mg; # fix broken multilines
	    $buf=~s/\\\n//mg; # merge multilines

	    for(split(/\n/, $buf))
	      {
		  next if(/^#/); # weed out whole comment lines immediately
		  next unless $_; # weed out empty lines immediately

		  s/#.*//; # kill off comments
		  s/^\s+//; # kill off leading spaces
		  s/\s+$//; # kill off trailing spaces
		  if (!/[a-z_6]{4,}\s*=/) { # Shortest configuration option is 4
		      # Invalid line
		      if ($file eq $config_file) {
			  # Main configfile: warn and skip just this line
			  warn "Invalid line in main configuration file $config_file: \Q$_\E. Ignoring line\n";
			  next;
		      }
		      else {
			  # conf.d file, skip file
			  warn "Invalid configuration line in $file: \Q$_\E. Skipping file\n";
			  next CONFIGFILE;
		      }
		  }

		  if (my ($key, $value) = split(/\s*=\s*/)) { # split into key and value pair
		      if ($key =~ /^_/) {
			  warn "Can't set private configuration option $key. Ignoring\n";
			  next;
		      }
		      $value = 0 unless ($value);
		      #print "key: $key, value: $value\n";
		      $config{$key} = $value;
		      #print "$config{$key}\n";
		  }
	      }
	}
	close $fh;
    }

    # Recognise old/renamed configuration options
    foreach (['logdir' => 'log_dir'], ['fetch_timeout' => 'data_timeout']) {
        if ($config{@$_[0]}) {
	    $config{@$_[1]} = $config{@$_[0]};
	    delete $config{@$_[0]};
	}
    }
    return \%config;
}

sub cfg_split {
    my ($item) = @_;
    return $item ? grep {length} # Remove empty
      map {(my $s = $_) =~ s#\\(?=[,;])##g; $s} # Normalise escaped separators
        split(/\s*(?<!\\)[,;]\s*/, $item) : undef;
}

sub private_config {

    if($cfg->{path_map}) {
	for(cfg_split($cfg->{path_map})) {
	    my @tmp = split(/\s+/, $_);
	    next unless my $key=shift(@tmp);
	    if (@tmp) {
		s#/+$## foreach @tmp; # No trailing /
		$cfg->{_path_map}{$key} = [@tmp];
	    }
	    else {
		# Unset predefined?
		delete $cfg->{_path_map}{$key}
	    }
	}
    }

    # Handle libcurl configuration
    if ($cfg->{libcurl}) {
        for (cfg_split($cfg->{libcurl})) {
	    my @tmp = split(/\s+/, $_, 2);
	    next unless my $key=uc(shift(@tmp));
	    if (@tmp) {
		$cfg->{_libcurl}{$key} = shift(@tmp);
	    }
	}
    }

    # Expand %PATH_MAP% in allowed_locations.
    # Still support legacy PATH_MAP.
    if ($cfg->{allowed_locations}) {
        $cfg->{allowed_locations} =~ s/(?:\b|%)PATH_MAP(?:%|\b)/join(', ', keys %{$cfg->{_path_map}})/ge;
    }

    # Handle regexp expansions
    my %valid_expansion = (
			   package_name => '[a-z0-9][-+.a-z0-9]*',
			   version => '(?:\d+:)?[0-9][-+:.~a-zA-Z0-9]*',
			   archs => '(?:' . join('|',
						 do {
						     # Check config list is valid architecure
						     my %v = map { $_ => 1 } Dpkg::Arch::get_valid_arches;
						     grep { warn "Ignoring invalid architecture in supported_archs: $_\n" unless $v{$_};
							    $v{$_};
							}
						       cfg_split($cfg->{supported_archs});
						 } , 'all') . ')',
			   ubuntu_release_names => '(?:' . join('|',
								# Only allow lowercase alphanumeric Ubuntu release names
								grep  { my $invalid = m%[^a-z]% && warn "Ignoring invalid Ubuntu release: $_\n";
									!$invalid ; # Return negated match status
								    }
								cfg_split($cfg->{ubuntu_release_names})) .
			   ')'
			  );
    foreach my $regexp_name (glob('{{{skip_,}checksum,index,installer,package,pdiff}_files,soap_url}_regexp')) {

	for ($cfg->{$regexp_name}) {

	    # Legacy support for UBUNTU_RELEASE_NAMES
	    s/\bUBUNTU_RELEASE_NAMES(?!%)/%VALID_UBUNTU_RELEASE_NAMES%/g if $regexp_name eq 'installer_files_regexp';

	    # Expand %VALID_*%
	    foreach my $key (keys %valid_expansion){
		s/%VALID_\U$key%/$valid_expansion{$key}/g
	    }

	}
    }


    if ($cfg->{interface}) {
	# If we can't resolve item, see if it is an interface name
	unless (Socket::inet_aton($cfg->{interface})) {
	    require IO::Interface::Simple;
	    my $if = IO::Interface::Simple->new($cfg->{interface});
	    if ($if) {
		$cfg->{interface} = $if->address;
	    }
	    else {
		$cfg->{interface} = '';
	    }
	}
    }

    # Proxy support
    for ($cfg->{http_proxy}) {
	last unless length;

	# Legacy use_proxy warning
	if (exists $cfg->{use_proxy} && !$cfg->{use_proxy}) {
	    warn 'Legacy use_proxy=0 is deprecated and is slated for removal. Unset http_proxy if upstream proxy not required';
	    last; # But still honour it
	}

	$cfg->{_proxy} = URI->new((m!^[^:/?#]+://! ? '' : 'http://') . $_);
	if ($cfg->{_proxy}) {
	    # Clear any path
	    $cfg->{_proxy}->path(undef);

	    # Legacy proxy authorisation
	    if ($cfg->{http_proxy_auth}) {

		# Legacy use_proxy_auth warning
		if (exists $cfg->{use_proxy_auth} && !$cfg->{use_proxy_auth}) {
		    warn 'Legacy use_proxy_auth=0 is deprecated and is slated for removal. Unset http_proxy_auth if upstream proxy authentication not required';
		}
		else {
		    if ($cfg->{_proxy}->userinfo) {
			warn 'Upstream proxy authorisation already set, ignoring deprecated http_proxy_auth';
		    }
		    else {
			$cfg->{_proxy}->userinfo($cfg->{http_proxy_auth});
		    }
		}
	    }
	}
	else {
	    info_message("Warning: failed to parse http_proxy $cfg->{http_proxy} as URI. Ignoring.");
	}
        # Hide auth details so they don't appear in /config
	m%[^/?#]+(?=@)% && substr ($_, $-[0], $+[0] - $-[0]) =~ tr/:/*/c;
    }
    # Hide legacy auth details so they don't appear in /config
    $cfg->{http_proxy_auth} =~ tr/:/*/c if exists $cfg->{http_proxy_auth};


    # Rate limit and disk_usage_limit support
    foreach (qw(limit disk_usage_limit)) {
	next unless exists $cfg->{$_};
	if (defined(my $e = expand_byte_suffix($cfg->{$_}))) { # Test defined() as 0 is valid
	    $cfg->{"_$_"} = $e;	# Set private variable
	}
	else {
	    warn "Unrecognised $_: $cfg->{$_}. Ignoring.";
	}
    }

    # convert curl_throttle from milliseconds to seconds
    $cfg->{_curl_throttle} = $cfg->{curl_throttle}/1000;
    return;
}

sub expand_byte_suffix {
    my ($bstring) = @_;
    my $ret;

    # The standards are pretty confused here between SI, IEC and JDEC.
    #
    # Rationale:
    # Pre-1.7 configuration was based on wget(1) (which uses lowercase k, m, g, t): decimal.
    # Standard SI prefixes: decimal.
    # Support *bibytes as binary
    # K on its own isn't SI, so: binary

    for ($bstring) {
	/^(\d+)$/ && do {$ret = $1; last};

	/^(\d+)\s*kB?$/ && do {$ret = $1 * 1000; last};
	/^(\d+)\s*Ki?B?$/ && do {$ret = $1 * 1024; last};

	/^(\d+)\s*[mM]B?$/ && do {$ret = $1 * 1000**2; last};
	/^(\d+)\s*MiB?$/ && do {$ret = $1 * 1024**2; last};

	/^(\d+)\s*[gG]B?$/ && do {$ret = $1 * 1000**3; last};
	/^(\d+)\s*GiB?$/ && do {$ret = $1 * 1024**3; last};

	/^(\d+)\s*[tT]B?$/ && do {$ret = $1 * 1000**4; last};
	/^(\d+)\s*TiB?$/ && do {$ret = $1 * 1024**4; last};
    }
    return $ret;
}

# check directories exist and are writable
# Needs to run as root as parent directories may not be writable
sub check_install {
    # Die if we have not been configured correctly
    die "$0: No cache_dir directory!\n" if (!-d $cfg->{cache_dir});

    my $uid = $cfg->{user}=~/^\d+$/ ? $cfg->{user} : POSIX::getpwnam($cfg->{user});
    my $gid = $cfg->{group}=~/^\d+$/ ? $cfg->{group} : POSIX::getgrnam($cfg->{group});

    if (!defined ($uid || $gid)) {
	die "Unable to get user:group";
    }

    my @dir = ($cfg->{cache_dir}, $cfg->{log_dir}, "$cfg->{cache_dir}/private",
		     "$cfg->{cache_dir}/import", "$cfg->{cache_dir}/packages",
		     "$cfg->{cache_dir}/headers");

    # Check pidfile and libcurl socket directories
    for (qw/_pidfile libcurl_socket/) {
        push @dir, (File::Spec->splitpath($cfg->{$_}))[1] if defined $cfg->{$_};
    };

    foreach my $dir (@dir) {
	if (!-d $dir) {
	    warn "Info: $dir missing. Doing mkdir -p $dir\n";
	    File::Path::make_path($dir, {user => $uid,
					 group => $gid}) || die "Unable to create $dir: $!";
	}
	if ((stat($dir))[4] != $uid || (stat(_))[5] !=  $gid) {
	    warn "Warning: $dir -- setting ownership to $uid:$gid\n";
	    chown ($uid, $gid, $dir) || die "Unable to set ownership for $dir: $!";
	}
    }
    for my $file ("$cfg->{log_dir}/access.log", "$cfg->{log_dir}/error.log") {
	if(!-e $file) {
	    warn "Warning: $file missing. Creating.\n";
	    open(my $tmp, '>', $file) || die "Unable to create $file: $!";
	    close($tmp);
	}
	if ((stat($file))[4] != $uid || (stat(_))[5] !=  $gid) {
	    warn "Warning: $file -- setting ownership to $uid:$gid\n";
	    chown ($uid, $gid, $file) || die "Unable to set ownership for $file: $!";
	}
    }
    return;
}

# Arg is ref to flattened hash. Returns hash ref
sub hashify {
    my ($href) = @_;
    return unless $$href;
    if ($$href =~ /^FrT;/) {
	# Deprecated format: FreezeThaw
	if (Module::Load::Conditional::check_install(module => 'FreezeThaw')) {
	    require FreezeThaw;
	    return (FreezeThaw::thaw($$href))[0];
	}
	else {
	    warn "Deprecated data serialisation format found. Requires libfreezethaw-perl to be installed\n";
	}
    }
    elsif (Storable::read_magic $$href) {
	# New format: Storable
	return Storable::thaw($$href);
    }
    elsif ($$href =~ /. ./) {
    	# Old format: join
	return {split(/ /, $$href)};
    } else {
	warn "Unrecognised serialisation method\n";
    }
    return;
}

# Get filename from filehandle
sub filename_fh {
    my ($fh) = @_;

    return readlink fd_path($fh);
}

# Get path of fildescriptor
# VERY Linux specific
sub fd_path {
    my ($fh) = @_;

    die 'Not a GLOB' unless ref $fh eq 'GLOB';
    return '/proc/self/fd/' . $fh->fileno;
}

# Delete cached files by filehandle
sub unlink_by_fh {
    my @fh = @_;

    my $count;

    foreach (@fh) {
	unless ((my $ref = ref) eq 'GLOB') {
	    warn "Not a GLOB, skipping: $ref \n";
	    next;
	}
	$_ =  filename_fh($_);
	next unless -f; # Skip already deleted
	debug_message("Deleting $_") if defined &debug_message && $cfg->{debug};
	($count += unlink $_) || warn "Failed to delete $_: $!";
    }
    return $count;
}


# Verbose wrapper to flock
sub _flock {
    my ($fh, $flags) = @_;

    my $ret;
#    unless ($ret = flock($fh, $flags | LOCK_NB)) {
#	if ($cfg->{debug}) {
    my $flags_str;
    my $caller_str;
    my $fh_name;
    my $dodebug = 0;
    if ($cfg->{debug_flock} && defined &debug_message) {
	    $fh_name=filename_fh($fh);
	    for my $file ("$cfg->{log_dir}/access.log", "$cfg->{log_dir}/error.log") {
		    if ( $fh_name eq $file ) { goto DOFLOCK; }
	    }
	    $dodebug = 1;

	    # Prepare debugging strings if asked to...
	    my ($package, $filename, $line, $subroutine,
		$hasargs, $wantarray, $evaltext, $is_require,
		$hints, $bitmask, $hinthash) = caller (1);
	    $caller_str = "_FLOCK($fh_name) called from "
#			    . $package . "::" 
			    . $filename 
			    . "[" . $line . "]" 
			    . $subroutine . "()"; 

	    # Hash for decoding flag symbols.
	    #
	    # __PACKAGE__->$sym references flock constants without needing to
	    # disable strict refs
	    my %h = map {$_ => __PACKAGE__->$_} glob("LOCK_{SH,EX,UN,NB}");
#	    debug_message('Waiting for '
#			  . join ('|', grep {$h{$_} & $flags && $_} keys %h)
#			  . ' on '
#			  . filename_fh($fh)) if defined &debug_message && $cfg->{debug};
	    $flags_str=join ('|', grep {$h{$_} & $flags && $_} keys %h);
	    # Filter out some (un)locks, like the log
	    # file we are writing into to report this line
	    if ( ($flags&LOCK_UN) ) {
		    debug_message("$caller_str : RELEASING");
#	    } else {
#		    debug_message("$caller_str : Trying to quickly get $flags_str");
	    }
    }
DOFLOCK:
    unless ($ret = flock($fh, $flags | LOCK_NB)) {
	if ($dodebug) {
	    debug_message("$caller_str : WAITING for $flags_str");
	}
        $ret = flock($fh, $flags);
#	debug_message('Got it!') if defined &debug_message && $cfg->{debug};
    }

    if ($dodebug) {
	if ($ret) {
	    if ( ($flags&LOCK_UN) ) {
		debug_message("$caller_str : RELEASED");
	    } else {
		debug_message("$caller_str : GOT $flags_str");
	    }
	} else {
	    if ( ($flags&LOCK_UN) ) {
		debug_message("$caller_str : FAILED to RELEASE");
	    } else {
		debug_message("$caller_str : FAILED to get $flags_str");
	    }
	}
    }

    return $ret;
}

# Argument is filehandle or filename, returns HTTP::Response
sub read_header {
    my ($file) = @_;
    my ($r, $fh);

    if (ref $file eq 'GLOB') {
	$fh = $file;
    }
    else {
	open($fh, '<', $file) || die "Open header $file failed: $!";
    }

    if ($fh) {
	for ($fh->getline) {
	    last unless defined;
	    if (/^(?:HTTP\/1\.[01]\s+)?\d{3}\s+/) { # Valid
		seek($fh,0,0) || die "Seek failed: $!";
		{
		    local $/; # Slurp
		    $r = HTTP::Response->parse(<$fh>);
		}
		chomp_message($r);
		# Fake Client-Date if not specified
		$r->client_date($r->date || time) unless $r->client_date;
		$r->header('Age' => $r->current_age);
	    }
	    else { # Invalid
		warn "Invalid/corrupt header file: $file";
		undef $r;
		undef $fh;
	    }
	}
    }

    # Don't explicitly close $fh
    return $r;
}

# Args are filename/filehandle and HTTP::Response
sub write_header {
    my ($file, $response) = @_;

    debug_message('Writing header') if defined &debug_message && $cfg->{debug};
    # Remove Connection header and options
    foreach ($response->header('Connection')) {
	$response->remove_header($_)
    }
    $response->remove_header('Connection','Age');
    if ($response->request &&
	(my $request_url = $response->request->uri)) {
	# Add Request URL to headers
	$response->header('X-AptCacher-URL' => $request_url);
    }
    $response->client_date(time); # HTTP::Response uses this to calculate current_age
    my $chfh;
    if (ref $file eq 'GLOB'){
	$chfh = $file;
	$chfh->truncate(0) || die "Truncate failed: $!";
	seek($chfh,0,0) || die "Seek failed: $!";
    }
    else {
	open ($chfh, '>', $file) || die "Unable to open $file, $!";
    }
    print $chfh $response->status_line, "\n";
    print $chfh $response->headers->as_string;
    # No explicit close. Rely on gc or explicit close in caller
    return;
}

# HTTP::Response->parse is leaving \r on the end of the message!
sub chomp_message {
    my ($r) = @_;

    for ($r->message) {
	last unless defined;
	local $/ = "\r";
	redo if chomp;
	$r->message($_);
    }
    return $r;
}

# Returns valid namespace from URI
sub get_namespace {
    my ($uri) = @_;

    if ($cfg->{distinct_namespaces}) {
	my @path = ($uri->host, $uri->path_segments);
	# Use path_map, if defined
	if (defined $cfg->{_path_map}{$path[0]}) {
	    return $path[0];
	}
	# Work from the end
	while (defined(local $_ = pop @path)) {
	    last if /^(?:pool|dists)$/;
	}
	return join('_', grep {length} @path);
    }
    return;
}

# Returns URI object of url used by libcurl to fetch file
sub get_upstream_url {
    my ($filename) = @_;
    my $uri;

    # Try cached headers first
    if (my $response = read_header("$cfg->{cache_dir}/headers/$filename")) {
	$uri = URI->new($response->header('X-AptCacher-URL'));
    }

    unless ($uri) {
	# Old complete file
	if (open (my $cfh, '<', my $complete_file = "$cfg->{cache_dir}/private/$filename.complete")) {
	    $uri = URI->new(<$cfh>);
	    close($cfh);
	}
	else {
	    # Assume same as request
	    $uri = get_original_url($filename);
	}
    }
    return $uri;
}

# Returns URI object of url used to request file
sub get_original_url {
    my ($filename) = @_;

    # Infer from filename, assume HTTP
    return URI->new('http://' . join('/', split(/_/, $filename)));
}

# Stores data flattened for use in tied hashes
# Arg $fh is optional
sub extract_sums {
   my ($name, $fh, $hashref) = @_;

   my $fh_opened = 0;
   if ($fh) {
       seek($fh,0,0) || die "Seek failed: $!";
   }
   else {
       open($fh, '<', $name) || die "Open $name failed: $!";
       $fh_opened = 1;
   }

   my $raw = IO::Uncompress::AnyUncompress->new($fh)
     or die "Decompression failed: $AnyUncompressError\n";

   # Name is just the cached filename without path
   $name = (File::Spec->splitpath($name))[2];

   # Determine namespace
   my $namespace;
   if ($namespace = get_namespace(get_original_url($name)) || ''){ # Default empty, not undef
       $namespace .= '/';
   }

   my ($indexbase) = ($name =~ /([^\/]+_)(?:Index|(?:In)?Release)$/);
   $indexbase = '' unless $indexbase; # Empty by default (for Sources)

   my %hash_length = (32 => 'md5', 40 => 'sha1', 64 => 'sha256');
   my ($skip,%data);
   while(<$raw>) {
       last if $AnyUncompressError;
       chomp;
       if (/^SHA\d+-Patches:/) {
	   $skip = 0;
       }
       elsif (/^SHA\d+-[a-zA-Z]+:/) {
	   # This flag prevents us bothering with unnecessary sections
	   # (History|Current|Download) of diff_Index files
	   $skip = 1;
       }
       elsif (/^\s+([a-z0-9]{32,64})\s+(\d+)\s(\S+)$/) { # diff_Index/Release/Sources
	   next if $skip;
	   my $hexdigest=$1;
	   my $size=$2;
	   my $file=$indexbase.$3;

	   $file=~s!/!_!g; # substitute any separators in indexed filename

	   if ($name =~ /Index$/) {
	       $file.=".gz";
	   }
	   elsif ($name =~ /_Sources(?:\.(?:x|g)z|\.bz2)?$/) {
	       # Prepend namespace, if set
	       $file = $namespace . $file;
	   }
	   $data{$file}{size} = $size;
	   { # Select algorithm based on hex length
	       my $len = length($hexdigest);
	       if (exists $hash_length{$len}) {
		   $data{$file}{$hash_length{$len}}=$hexdigest;
	       } else {
		   warn "Unrecognised algorithm length: $len. Ignoring.";
	       }
	   }
       }
       elsif(/^MD5sum:\s+([a-z0-9]{32})$/) { # Packages
	   $data{md5}=$1;
       }
       elsif(/^SHA1:\s+([a-z0-9]{40})$/) {
	   $data{sha1}=$1;
       }
       elsif(/^SHA256:\s+([a-z0-9]{64})$/) {
	   $data{sha256}=$1;
       }
       elsif(/^Size:\s+([0-9]+)$/) {
	   $data{size}=$1;
       }
       elsif(/^Filename:\s+.*?([^\/]+)$/) { # Non-greedy quantifier essential
	   # Prepend namespace, if set
	   $data{file} = $namespace . $1;
       }
   }
   continue {
       # diff_Index and Release files have no empty line at the end, so also
       # test eof() for them
       if(!length || $raw->eof()) { # End of record/file
	   if (exists $data{file}) {
	       # From Packages. Convert to hash of hashes with filename as key
	       foreach (qw(size md5 sha1 sha256)) {
		   $data{$data{file}}{$_} = $data{$_};
		   delete $data{$_};
	       }
	       delete $data{file};
	   }

	   while (my($key,$value) = each %data) {
	       $hashref->{$key} = Storable::freeze($value);
	   }
	   undef %data; # Reset
       }
   };
   if ($fh_opened) {
       close($fh) || warn "Problem closing $name";
   }
   if ($AnyUncompressError) {
       warn "$name Read failed: $AnyUncompressError. Aborting read\n";
       return;
   }
   return 1;
}

{ # Scoping block
    my $glock;

    sub set_global_lock {
	my ($msg)=@_;

	my $glockfile="$cfg->{cache_dir}/private/glock";

	$msg='Unspecified' if !$msg;

	debug_message("Global lock: \u$msg") if defined &debug_message && $cfg->{debug};

	# May need to create it if the file got lost
	sysopen($glock, $glockfile, O_CREAT) || die "Unable to open lockfile: $!";
	_flock($glock, LOCK_EX) || die "Unable to lock $glockfile for \u$msg: $!";
	return defined($glock);
    }

    sub release_global_lock {
	unless ($glock->opened) {
	    carp('Attmept to free lock not held');
	    return;
	}
	_flock($glock, LOCK_UN) || die "Unable to release lock: $!";
	close $glock || die "Unable to close lock: $!";
	debug_message("Release global lock") if defined &debug_message &&  $cfg->{debug};
	return;
    }

    sub global_lock_fh {
	return $glock;
    }
}


sub setup_ownership {
    my $uid=$cfg->{user};
    my $gid=$cfg->{group};

    if($cfg->{_chroot}) {
	if($uid || $gid) {
	    # open them now, before it is too late
	    # FIXME: reopening won't work, but the lose of file handles needs to be
	    # made reproducible first
	    open_log_files();
	}
	chroot $cfg->{_chroot} || die "Unable to chroot: $1";
	chdir $cfg->{_chroot};
    }

    if($gid) {
	if($gid=~/^\d+$/) {
	    my $name = POSIX::getgrgid($gid);
	    die "Unknown group ID: $gid (exiting)\n" if !$name;
	}
	else {
	    $gid = POSIX::getgrnam($gid);
	    die "No such group (exiting)\n" if !defined($gid);
	}
	POSIX::setgid($gid) || die "setgid failed: $!";
	$) =~ /^$gid\b/ && $( =~ /^$gid\b/ || die "Unable to change group id";
    }

    if($uid) {
	if($uid=~/^\d+$/) {
	    my $name = POSIX::getpwuid($uid);
	    die "Unknown user ID: $uid (exiting)\n" if !$name;
	}
	else {
	    $uid = POSIX::getpwnam($uid);
	    die "No such user (exiting)\n" if !defined($uid);
	}
	POSIX::setuid($uid) || die "setuid failed: $!";
	$> == $uid && $< == $uid || die "Unable to change user id";
    }
    return;
}

sub is_file_type {
    my ($type,$file) = @_;
    $type .= '_files_regexp';
    die "Regexp $type not defined in config" if !exists($cfg->{$type});
#    debug_message("is_file_type() matching file '$file' against '$type' regex '$cfg->{$type}'") if defined &debug_message && $cfg->{debug};
    return ($file =~ $cfg->{$type});
}

sub load_checksum {
    return unless $cfg->{checksum};
    if (Module::Load::Conditional::check_install(module => 'BerkeleyDB')) {
	require('apt-cacher-cs.pl');
    }
    else {
	warn "Checksum disabled as BerkeleyDB not found. Install libberkeleydb-perl\n";
	$cfg->{checksum}=0;
    }
    return;
}

######### HOOKS ###########
#
# arg: file to be scanned and added to DB
sub import_sums {
   return 1;
}

# purpose: ?create?, lock the DB file and establish DB connection
sub db {
   return 1;
}

# args: filehandle and DB handle
sub check_sum {
   return 1;
}

1;
