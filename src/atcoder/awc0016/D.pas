program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, q, i, j, l, r, a: int32;
	k: int64;
	sa, sf: array [0 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k, q);

	sa[0] := 0;
	for i := 1 to n do begin
		read(a);
		sa[i] := sa[i-1] + a;
	end;
	readln;

	sf[0] := 0;
	r := 1;
	for l := 1 to n do begin
		while (r < n) and (sa[r] - sa[l-1] <= k) do inc(r);
		sf[l] := sf[l-1] + r;
	end;

	for j := 1 to q do begin
		readln(l, r);
		writeln(sf[r] - sf[l-1]);
	end;

end.
