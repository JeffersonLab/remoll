#!/bin/bash
#
# You must source this script.
#   - It requires JLAB_ROOT and JLAB_VERSION to be set to something sensible
#   - At your bash prompt type: ". <path>/jlab.sh"
#
# Get the JLAB environment variables by parsing the script in tcsh and then
# printing the  environment.  We need to filter out the spaces, since that
# confuses the for loop. We also need to drop some tcsh specific environment
# variables from the export list.
#
# Maintainer:  Brad Sawatky <brads@jlab.org>  (Nov 17, 2016)
#   - rework of script by Maurik Holtop

# set JLAB_VERSION to the wanted tag if it's not set yet
: ${JLAB_VERSION=2.3}

TMPF=`mktemp`
# start with empty environment
env -i tcsh ${TCSH_ARG} -c "
    set JLAB_ROOT=${JLAB_ROOT} ;
    set JLAB_VERSION=${JLAB_VERSION} ;
    set PATH=${PATH} ;
    set LD_LIBRARY_PATH=${LD_LIBRARY_PATH} ;
    set DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH} ;
    set home=${HOME} ;
    source ${JLAB_ROOT}/${JLAB_VERSION}/ce/jlab.csh $1 ;
    printenv" | perl -e '
  my %IGNORE;
  ## Add variables to ignore to the space-delimited list below
  foreach( qw(
    HISTTIMEFORMAT SHELL PS1 overwrite _ BASH_FUNC_module\(\)
  )){ $IGNORE{$_}="yes"; }

  while(<>) {
    chomp;

    ## Look for and treat variables from printenv here
    if(/^([_a-zA-Z0-9]*?)=(.*)$/) {
      my $var = $1;
      my $arg = $2;

      next if( exists($IGNORE{$var}) );

      if( $ENV{OSTYPE} eq "darwin" ) {
        if( $var =~ /(DY)?LD_LIBRARY_PATH/ ) {
          $arg =~ s/:$//;   # strip trailing colon
          print "export ${var}=\"${arg}:$ENV{$var}\"\n";
        }
        next;
      }

      ## DEFAULT is to re-export the variable list into the bash environment
      print "export ${var}=\"${arg}\"\n";
      next;
    }

    ## Dump information messages (and everything else) to STDERR where it should not hurt anyone
    print STDERR "$_\n";
  }
' >> $TMPF

source "$TMPF"
rm -f "$TMPF"
