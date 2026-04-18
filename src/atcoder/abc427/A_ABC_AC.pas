program A_ABC_AC;
{$MODE DELPHI}
uses
	math;
var
	n, i, h: int32;
	s: string;

begin
	readln(s);
	n := length(s);
	h := (n+1) div 2;

	for i := 1 to n do
		if i <> h then write(s[i]);
	writeln;
end.
