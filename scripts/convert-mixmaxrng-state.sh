#!/bin/bash

if [ $# -lt 1 ] ; then
	echo "Convert the MixMaxRNG engine state stream format into rndm file format."
	echo "Usage (to stdout): `basename $0` [infile]"
	exit
fi

# Convert file format like
#
#MixMaxRng-begin 30331785
#58933861902009719
#1785835428502820402
#1423853375428307990
#177508786414027104
#605196243949405736
#861701010447754611
#2250242789326832084
#1084033761752659819
#1855444124088891051
#242731982633356449
#808181319309293506
#1276833616859972515
#632930668351273437
#1854356059141373284
#1573087033096440666
#1508189008866546922
#2274580468617938516
#1
#1826895464979352203
#MixMaxRng-end
#
# written by
#
#std::ostream & MixMaxRng::put ( std::ostream& os ) const
#{
#   char beginMarker[] = "MixMaxRng-begin";
#   char endMarker[]   = "MixMaxRng-end";
#
#   int pr = os.precision(24);
#   os << beginMarker << " ";
#   os << theSeed << "\n";
#   for (int i=0; i<rng_get_N(); ++i) {
#      os <<  S.V[i] << "\n";
#   }
#   os << S.counter << "\n";
#   os << S.sumtot << "\n";
#   os << endMarker << "\n";
#   os.precision(pr);
#   return os;
#}
#
# to file format like
#
#mixmax state, file version 1.0
#N=17; V[N]={428696617498718304, 2003704315588530340, 7947374056829315, 2067466587362700185, 1326222523634245003, 1332396546628397551, 1896238309733840482, 1762831144035715774, 1107589152016452107, 695071895453953411, 38056132400761332, 367356884968403023, 280167416724960677, 1069286741314982869, 1528898768234620464, 1682222665550640926, 96744019811349342}; counter=5; sumtot=1549996030519243448;
#
# written by
#
#     int j;
#     fprintf(fh, "mixmax state, file version 1.0\n" );
#     fprintf(fh, "N=%u; V[N]={", rng_get_N() );
#     for (j=0; (j< (rng_get_N()-1) ); j++) {
#         fprintf(fh, "%llu, ", S.V[j] );
#     }
#     fprintf(fh, "%llu", S.V[rng_get_N()-1] );
#     fprintf(fh, "}; " );
#     fprintf(fh, "counter=%u; ", S.counter );
#     fprintf(fh, "sumtot=%llu;\n", S.sumtot );
#

# header line
echo "mixmax state, file version 1.0"
# remove begin/end markers
grep -v "MixMaxRng" $1 | \
	xargs | \
	gawk '{print("N="NF-2"; V[N]={"$1", "$2", "$3", "$4", "$5", "$6", "$7", "$8", "$9", "$10", "$11", "$12", "$13", "$14", "$15", "$16", "$17"}; counter="$18"; sumtot="$19";")}'
