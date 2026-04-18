program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	if s = 'tourist' then
		writeln(3858)
	else if s = 'ksun48' then
		writeln(3679)
	else if s = 'Benq' then
		writeln(3658)
	else if s = 'Um_nik' then
		writeln(3648)
	else if s = 'apiad' then
		writeln(3638)
	else if s = 'Stonefeang' then
		writeln(3630)
	else if s = 'ecnerwala' then
		writeln(3613)
	else if s = 'mnbvmar' then
		writeln(3555)
	else if s = 'newbiedmy' then
		writeln(3516)
	else if s = 'semiexp' then
		writeln(3481);
end.
