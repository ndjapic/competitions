program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	classes, sysutils;
var
	n, i: int8;
	s, ios: string;
	c1, c2: char;
	sl: tstringlist;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(ios);
	readln(s);

	sl := tstringlist.create;
	sl.delimiter := ' ';
	sl.delimitedtext := ios;
	n := strtoint(sl[0]);
	c1 := sl[1][1];
	c2 := sl[2][1];

	for i := 1 to n do
		if s[i] = c1 then
			write(c1)
		else
			write(c2);

	writeln;
	sl.free;
end.
