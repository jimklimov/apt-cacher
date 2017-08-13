#! /usr/bin/perl
#
# lib/apt-cacher.pl
#
# This is a library file for apt-cacher to allow code common to apt-cacher
# itself plus its supporting scripts to be maintained in one location.

use strict;
use warnings;
use POSIX ();
use Fcntl qw/:DEFAULT :flock/;
use FreezeThaw qw(freeze thaw);
use HTTP::Response;
use URI;
use IO::Uncompress::AnyUncompress qw($AnyUncompressError);
use Module::Load::Conditional;
use File::Spec;
use Carp;
our $cfg;

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
		  http_proxy_auth => '',
		  limit => 0,
		  limit_global => 0,
		  log_dir => '/var/log/apt-cacher',
		  request_empty_lines => 5,
		  request_timeout => 30,
		  return_buffer_size => 1048576, # 1Mb
		  reverse_path_map => 1,
		  ubuntu_release_names => join(', ', qw(dapper edgy feisty gutsy hardy intrepid jaunty karmic lucid maverick natty oneiric precise quantal)),
		  use_proxy => 0,
		  use_proxy_auth => 0,
		  user => $>,

		  # Private
		  _config_file => $config_file,
		  _path_map => {
				'debian-changelogs' => ['packages.debian.org'],
				'ubuntu-changelogs' => ['changelogs.ubuntu.com']
			       },

		  # Regexps
		  checksum_files_regexp => '^(?:' . join('|',
							 qw(Packages(?:\.gz|\.bz2)?
							    Sources(?:\.gz|\.bz2)?
							    (?:In)?Release
							    Index(?:\.bz2)?
							  )
							) . ')$',
		  skip_checksum_files_regexp => '^(?:' . join('|',
							qw((?:In)?Release
							   Release\.gpg
							 )
						       ) . ')$',
		  index_files_regexp => '^(?:' . join('|',
						      qw(Index(?:\.bz2)?
							 Packages(?:\.gz|\.bz2)?
							 Release(?:\.gpg)?
							 InRelease
							 Sources(?:\.gz|\.bz2)?
							 Contents-(?:[a-z]+-)?[a-zA-Z0-9]+\.gz
							 (?:srclist|pkglist)\.[a-z-]+\.bz2
							 release(?:\.gz|\.bz2)?
						       ),
						      # This needs to be a separate item to avoid a warning from the
						      # comma within qw()
						      q(Translation-[a-z]{2,3}(?:_[A-Z]{2})?(?:\.gz|\.bz2|\.xz)?)
						     ) . ')$',
		  installer_files_regexp => '^(?:' . join('|',
							  qw(vmlinuz
							     linux
							     initrd\.gz
							     changelog
							     NEWS.Debian
							     UBUNTU_RELEASE_NAMES\.tar\.gz(?:\.gpg)?
							     (?:Devel|EOL)?ReleaseAnnouncement(?:\.html)?
							     meta-release(?:-lts)?(?:-(?:development|proposed))?
							   )
							 ) . ')$',
		  package_files_regexp => '(?:' . join('|',
						       qw(^[-+.a-z0-9]+_(?:\d:)?[-+.~a-zA-Z0-9]+(?:_[-a-z0-9]+\.(?:u|d)?deb|\.dsc|\.tar(?:\.gz|\.bz2|\.xz)|\.diff\.gz)
							  \.rpm
							  index\.db-.+\.gz
							  \.jigdo
							  \.template
							)
						      ) .')$',
		  pdiff_files_regexp => '^2\d{3}-\d{2}-\d{2}-\d{4}\.\d{2}\.gz$',
		  soap_url_regexp => '^(?:http://)?bugs\.debian\.org(?::80)?/cgi-bin/soap.cgi$',
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
    return $item ? grep {!/^$/} split(/\s*[,;]\s*/, $item) : undef;
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
	    my @tmp = split(/\s+/, $_);
	    next unless my $key=uc(shift(@tmp));
	    if (@tmp) {
		$cfg->{_libcurl}{$key} = shift(@tmp);
	    }
	}
    }

    # Expand PATH_MAP in allowed_hosts
    if ($cfg->{allowed_locations}) {
        $cfg->{allowed_locations} =~ s/\bPATH_MAP\b/join(', ', keys %{$cfg->{_path_map}})/ge;
    }

    # Expand UBUNTU_RELEASE_NAMES in installer_files_regexp
    $cfg->{installer_files_regexp} =~ s/UBUNTU_RELEASE_NAMES/'(?:'
      . join('|',
	     grep { m%[^a-z]% ? warn "Ignoring invalid Ubuntu release: $_\n" : $_ }
	     cfg_split($cfg->{ubuntu_release_names}))
	. ')'/ge;

    # Precompile regexps so they will not be recompiled each time
    $cfg->{$_} = qr/$cfg->{$_}/ foreach glob('{{{skip_,}checksum,index,installer,package,pdiff}_files,soap_url}_regexp');


    if ($cfg->{interface}) {
	# If we can't resolve item, see if it is an interface name
	unless (inet_aton($cfg->{interface})) {
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
    foreach ('proxy', 'proxy_auth') {
        if ($cfg->{"use_$_"} && !$cfg->{"http_$_"}) {
	    warn "use_$_ specified without http_$_ being set. Disabling.";
	    $cfg->{"use_$_"}=0;
	}
    }

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
    foreach my $dir (@dir) {
	if (!-d $dir) {
	    print "Info: $dir missing. Doing mkdir($dir, 0755)\n";
	    mkdir($dir, 0755) || die "Unable to create $dir: $!";
	}
	if ((stat($dir))[4] != $uid || (stat(_))[5] !=  $gid) {
	    print "Warning: $dir -- setting ownership to $uid:$gid\n";
	    chown ($uid, $gid, $dir) || die "Unable to set ownership for $dir: $!";
	}
    }
    for my $file ("$cfg->{log_dir}/access.log", "$cfg->{log_dir}/error.log") {
	if(!-e $file) {
	    print "Warning: $file missing. Creating.\n";
	    open(my $tmp, '>', $file) || die "Unable to create $file: $!";
	    close($tmp);
	}
	if ((stat($file))[4] != $uid || (stat(_))[5] !=  $gid) {
	    print "Warning: $file -- setting ownership to $uid:$gid\n";
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
	# New format: FreezeThaw
	return (thaw($$href))[0];
    } elsif ($$href =~ /. ./) {
    	# Old format: join
	return {split(/ /, $$href)};
    } else {
	return;
    }
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
    return '/dev/fd/' . $fh->fileno;
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
    unless ($ret = flock($fh, $flags | LOCK_NB)) {
	if ($cfg->{debug}) {
	    # Hash for decoding flag symbols.
	    #
	    # __PACKAGE__->$sym references flock constants without needing to
	    # disable strict refs
	    my %h = map {$_ => __PACKAGE__->$_} glob("LOCK_{SH,EX,UN,NB}");
	    debug_message('Waiting for '
			  . join ('|', grep {$h{$_} & $flags && $_} keys %h)
			  . ' on '
			  . filename_fh($fh)) if defined &debug_message && $cfg->{debug};
	}
        $ret = flock($fh, $flags);
	debug_message('Got it!') if defined &debug_message && $cfg->{debug};
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
	    if (/^(HTTP\/1\.[01]\s+)?\d{3}\s+/) { # Valid
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
	return join('_', grep {!/^$/} @path);
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

   if ($fh) {
       seek($fh,0,0) || die "Seek failed: $!";
   }
   else {
       open($fh, '<', $name) || die "Open $name failed: $!";
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

   my ($skip,%data);
   while(<$raw>) {
       last if $AnyUncompressError;
       chomp;
       # This flag prevents us bothering with the History section of diff_Index files
       if (/^SHA1-(?:Current|History)/) {
	   $skip = 1;
       }
       elsif (/^SHA1-Patches:/) {
	   $skip = 0;
       }
       elsif (/^\s(\w{32}|\w{40}|\w{64})\s+(\d+)\s(\S+)$/) { # diff_Index/Release/Sources
	   next if $skip;
	   my $hexdigest=$1;
	   my $size=$2;
	   my $file=$indexbase.$3;

	   $file=~s!/!_!g; # substitute any separators in indexed filename

	   if ($name =~ /Index$/) {
	       $file.=".gz";
	   }
	   elsif ($name =~ /_Sources(?:\.gz|\.bz2)?$/) {
	       # Prepend namespace, if set
	       $file = $namespace . $file;
	   }
	   $data{$file}{size} = $size;
	   for (my $len = length($hexdigest)) { # Select algorithm based on hex length
	       $len == 32 # md5
		 && do { $data{$file}{md5}=$hexdigest; last; };
	       $len == 40 # sha1
		 && do { $data{$file}{sha1}=$hexdigest; last; };
	       $len == 64 # sha256
		 && do { $data{$file}{sha256}=$hexdigest; last; };
	       warn "Unrecognised algorithm length: $len. Ignoring.";
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

       # diff_Index and Release files have no empty line at the end, so test eof() for them
       if(/^$/ || ($name =~ /(?:(?:In)?Release|diff_Index)$/ && $raw->eof())) { # End of record/file
	   if (exists $data{file}) {
	       # From Packages. Convert to hash of hashes with filename as key
	       foreach (qw(size md5 sha1 sha256)) {
		   $data{$data{file}}{$_} = $data{$_};
		   delete $data{$_};
	       }
	       delete $data{file};
	   }

	   foreach (keys %data) {
	       $hashref->{$_} = freeze($data{$_});
	   }
	   undef %data; # Reset
       }
   };
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

    if($cfg->{chroot}) {
	if($uid || $gid) {
	    # open them now, before it is too late
	    # FIXME: reopening won't work, but the lose of file handles needs to be
	    # made reproducible first
	    open_log_files();
	}
	chroot $cfg->{chroot} || die "Unable to chroot: $1";
	chdir $cfg->{chroot};
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

# Still matches against the filename only if called with a fully qualified path
sub is_file_type {
    my ($type,$file) = @_;
    $type .= '_files_regexp';
    die "Regexp $type not defined in config" if !exists($cfg->{$type});
    return ((File::Spec->splitpath($file))[2] =~ $cfg->{$type});
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
