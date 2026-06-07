program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	classes, sysutils;
const
	NN = 100;
var
	n, i, i0: int8;
	sl: tstringlist;
	ios: string;
	a: array [1 .. NN] of int32;
	s: array [1 .. NN] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	sl := tstringlist.create;
	sl.delimiter := ' ';
	i0 := 1;

	for i := 1 to n do begin
		readln(ios);
		sl.delimitedtext := ios;
		s[i] := sl[0];
		a[i] := strtoint(sl[1]);
		if a[i] < a[i0] then i0 := i;
	end;

	for i := i0 to n do writeln(s[i]);
	for i := 1 to i0-1 do writeln(s[i]);

	sl.free;
end.
