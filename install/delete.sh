#! /bin/sh

set -e

case "$1" in
    purge)
		defaultfile='/etc/default/apt-cacher'
		# we mimic dpkg as closely as possible, so we remove configuration
		# files with dpkg and ucf backup extensions too:
		### Some of the following is from Tore Anderson:
		for ext in '~' '%' .bak .dpkg-tmp .dpkg-new .dpkg-old .dpkg-dist .ucf-new .ucf-old .ucf-dist '';  do
		    rm -f $defaultfile$ext
		done
 
		# and finally clear it out from the ucf database
		if which ucf >/dev/null; then
		    ucf --purge $defaultfile
		fi    
		if which ucfr >/dev/null; then
		    ucfr --purge apt-cacher $defaultfile
		fi    

		rm -rf /var/cache/apt-cacher /var/log/apt-cacher
	;;
	
esac

# Automatically added by dh_installdeb
dpkg-maintscript-helper rm_conffile /etc/cron.daily/apt-cacher 1.7.1 -- "$@"
# End automatically added section
# Automatically added by dh_installinit
if [ "$1" = "purge" ] ; then
	update-rc.d apt-cacher remove >/dev/null
fi
# End automatically added section
# Automatically added by dh_installdebconf
if [ "$1" = purge ] && [ -e /usr/share/debconf/confmodule ]; then
	. /usr/share/debconf/confmodule
	db_purge
fi
# End automatically added section


exit 0


