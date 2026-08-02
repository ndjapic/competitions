program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	XX = 1000;
var
	n, i, a, b, s, ans: int32;
	seen: array [1 .. XX] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for s := 1 to XX do seen[s] := false;

	for a := 1 to (XX - 3) div 7 do begin
		b := 1;
		// s := 4*a*b + 3*a + 3*b;
		s := 7*a+3;

		// 4s = (4a)(4b) + 3(4a) + 3(4b)
		// 4s+9 = (4a)(4b) + 3(4a) + 3(4b) + 9
		// 4s+9 = (4a)(4b+3) + 3(4b+3)
		// 4s+9 = (4a+3)(4b+3)
		// 4s = (4a+3)(4b+3) - 9

		while s <= XX do begin
			seen[s] := true;
			inc(b);
			// s := 4*a*b + 3*a + 3*b;
			inc(s, 4*a+3);
		end;
	end;

	ans := n;
	for i := 1 to n do begin
		read(s);
		if seen[s] then dec(ans);
	end;
	readln;
	writeln(ans);
end.
