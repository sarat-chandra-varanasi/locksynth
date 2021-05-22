touch empty
rm merge.pl
cat *.pl > merge.pl
swipl -L0 -G0 -T0 --goal=synth --stand_alone=true -o locksynth -c merge.pl
