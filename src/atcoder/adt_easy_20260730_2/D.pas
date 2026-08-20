program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	XX = 1000;
var
	n, i, s, a, b, ans: int32;
	seen: array [1 .. XX] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for s := 1 to XX do seen[s] := false;

	for a := 1 to XX do begin
		b := 1;
		s := 4*a*b + 3*a + 3*b;
		while s <= XX do begin
			seen[s] := true;
			inc(b);
			s := 4*a*b + 3*a + 3*b;
		end;
	end;

	ans := 0;
	for i := 1 to n do begin
		read(s);
		if not seen[s] then inc(ans);
	end;
	readln;

	writeln(ans);
end.
