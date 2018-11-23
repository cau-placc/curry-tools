#!/bin/sh
# This shell script updates some tools by downloading the current version
# from the Curry package repository.
#
# Note that the execution of this script requires an already installed 'cypm'!

# use local Curry executable if it exists (e.g., we are inside the distro):
CURRYBIN=`pwd`/../bin/curry
if [ -x "$CURRYBIN" ] ; then
  CPMOPTS="-d curry_bin=$CURRYBIN"
else
  CPMOPTS=
fi
CPM="cypm $CPMOPTS"

##############################################################################
echo "Updating 'cpm'..."
mv cpm/Makefile Makefile.cpm  # keep old Makefile
mv cpm/vendor/cass/src/CASS/PackageConfig.curry CASS_PackageConfig.curry
rm -rf cpm
$CPM checkout cpm
cd cpm
rm -rf .git* bin package.json
make fetchdeps
rm -rf vendor/*/.git*
rm -rf dependencies.txt fetch-dependencies.sh Makefile
cd ..
mv Makefile.cpm cpm/Makefile
mv CASS_PackageConfig.curry cpm/vendor/cass/src/CASS/PackageConfig.curry
echo "'cpm' updated from package repository."

##############################################################################
echo "Updating 'optimize'..."
mv optimize/Makefile Makefile.optimize  # keep old Makefile
mv optimize/.cpm/packages/cass/src/CASS/PackageConfig.curry CASS_PackageConfig.curry
mv optimize/package.json optimize_package.json
rm -rf optimize
$CPM checkout transbooleq
mv transbooleq optimize
cd optimize
$CPM install --noexec
rm -rf .git*
rm -rf .cpm/*_cache
rm -rf .cpm/packages/*/.git*
cd .cpm/packages
 CANAV=`ls -d cass-analysis-*`
 mv $CANAV cass-analysis
 CASSV=`ls -d cass-*\.*\.*`
 mv $CASSV cass
 ln -s cass-analysis $CANAV
 ln -s cass $CASSV
 PKGV=`ls -d flatcurry-*`
 mv $PKGV flatcurry
 ln -s flatcurry $PKGV
 PKGV=`ls -d csv-*`
 mv $PKGV csv
 ln -s csv $PKGV
 PKGV=`ls -d xml-*`
 mv $PKGV xml
 ln -s xml $PKGV
 PKGV=`ls -d scc-*`
 mv $PKGV scc
 ln -s scc $PKGV
 PKGV=`ls -d frontend-exec-*`
 mv $PKGV frontend-exec
 ln -s frontend-exec $PKGV
 PKGV=`ls -d propertyfile-*`
 mv $PKGV propertyfile
 ln -s propertyfile $PKGV
cd ../..
cd ..
mv Makefile.optimize optimize/Makefile
mv CASS_PackageConfig.curry optimize/.cpm/packages/cass/src/CASS/PackageConfig.curry
mv optimize_package.json optimize/package.json
echo "'optimize' updated from package repository."
