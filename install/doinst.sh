#! /bin/sh
# postinst script for apt-cacher
#
# see: dh_installdeb(1)

set -e

# summary of how this script can be called:
#        * <postinst> `configure' <most-recently-configured-version>
#        * <old-postinst> `abort-upgrade' <new version>
#        * <conflictor's-postinst> `abort-remove' `in-favour' <package>
#          <new-version>
#        * <deconfigured's-postinst> `abort-deconfigure' `in-favour'
#          <failed-install-package> <version> `removing'
#          <conflicting-package> <version>
# for details, see http://www.debian.org/doc/debian-policy/ or
# the debian-policy package
#
# quoting from the policy:
#     Any necessary prompting should almost always be confined to the
#     post-installation script, and should be protected with a conditional
#     so that unnecessary prompting doesn't happen if a package's
#     installation fails and the `postinst' is called with `abort-upgrade',
#     `abort-remove' or `abort-deconfigure'.


# Source debconf library.
. /usr/share/debconf/confmodule

case "$1" in
    reconfigure|configure)

	# Handle upgrade from checksumming.conf

	CONFDIR=/etc/apt-cacher
	CONFFILE=$CONFDIR/apt-cacher.conf

	if [ -f $CONFDIR/checksumming.conf ]; then
	    if grep -E -q "^[[:space:]]*require.+apt-cacher-lib-cs.pl" $CONFDIR/checksumming.conf; then

		echo "Importing checksum config from $CONFDIR/checksumming.conf"

		if  grep -E -q "^[#[:space:]]*checksum=" $CONFFILE ; then
		    TEMP_FILE=`mktemp -p /tmp`
		    sed 's/^[#[:space:]]*checksum=.*$/checksum=1/' $CONFFILE > $TEMP_FILE
		    mv $TEMP_FILE $CONFFILE
		    chmod 0644 $CONFFILE
		else
		    echo "checksum=1" >> $CONFFILE
		fi
	    fi
	    echo "Deleting obsolete file $CONFDIR/checksumming.conf"
	    rm $CONFDIR/checksumming.conf
	fi
	
	echo "Running apt-cacher's install script..."
	/usr/share/apt-cacher/install.pl

	defaultfile='/etc/default/apt-cacher'
		
	ucf --debconf-ok /usr/share/apt-cacher/default/apt-cacher $defaultfile
	ucfr apt-cacher $defaultfile

	db_get apt-cacher/mode
	case "$RET" in
	    daemon)
		echo "Setup apt-cacher running as standalone daemon."
		update-inetd --remove "3142\s.+/usr/sbin/apt-cacher" # PCRE
		sed -i 's/^[#[:space:]]*AUTOSTART=.*$/AUTOSTART=1/' $defaultfile # POSIX.2 RE
		;;
	    inetd)
		echo "Setup apt-cacher running from /etc/inetd.conf."
		update-inetd --add "3142\tstream\ttcp\tnowait\twww-data\t/usr/sbin/apt-cacher\tapt-cacher\t-i"
		sed -i 's/^[#[:space:]]*AUTOSTART=.*$/AUTOSTART=0/' $defaultfile # POSIX.2 RE
		;;
	    manual)
	        # Disable inetd and daemon
		update-inetd --remove "3142\s.+/usr/sbin/apt-cacher" # PCRE
		sed -i 's/^[#[:space:]]*AUTOSTART=.*$/AUTOSTART=0/' $defaultfile # POSIX.2 RE
		cat <<EOF 
Manual configuration of apt-cacher startup selected. No changes made.

If you are operating legacy CGI mode, this is deprecated and you really should
consider changing to one of the daemon modes. See cgi_advise_to_use and
cgi_redirect in man apt-cacher(8) for help in redirecting your clients.

To change your choice, run 

  dpkg-reconfigure apt-cacher

EOF
		;;
	esac

	;;

    abort-upgrade|abort-remove|abort-deconfigure)

    ;;

    *)
        echo "postinst called with unknown argument \`$1'" >&2
        exit 1
    ;;
esac

# dh_installdeb will replace this with shell code automatically
# generated by other debhelper scripts.

# Automatically added by dh_installinit
if [ -x "/etc/init.d/apt-cacher" ]; then
	update-rc.d apt-cacher defaults >/dev/null
	invoke-rc.d apt-cacher start || exit $?
fi
# End automatically added section
# Automatically added by dh_installdeb
dpkg-maintscript-helper rm_conffile /etc/cron.daily/apt-cacher 1.7.1 -- "$@"
# End automatically added section


db_stop
exit 0


