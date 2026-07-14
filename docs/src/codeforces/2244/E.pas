program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 200 * 1000;
var
	notc, tci, n, i, q, j, l, r, k: int32;
	s: string;
	a: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n, q);
		readln(s);

		a[1] := 0;
		for i := 2 to n do begin
			a[i] := a[i-1];
			if s[i-1] = s[i] then inc(a[i]);
		end;

		for j := 1 to q do begin
			readln(l, r, k);

			if a[r] - a[l] <= 2*k then
				writeln('YES')
			else
				writeln('NO');
		end;

	end;
end.
