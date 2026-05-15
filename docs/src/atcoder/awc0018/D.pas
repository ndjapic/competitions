program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #unsolved
uses
	math;
const
	nn = 18;
var
	n, k, u, v, c: int8;
	m, j: int16;
	b, mask: int32;
	team, ans: int64;
	a: array [0 .. nn] of int32;
	issue: array [0 .. nn, 0 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, k);

	for u := 0 to n-1 do begin
		read(a[u]);
		for v := u+1 to n-1 do issue[u, v] := 0;
	end;
	readln;

	for j := 1 to m do begin
		readln(u, v, b);
		issue[u-1, v-1] := b;
	end;

	ans := low(int64);
	for mask := 1 to (1 shl n) - 1 do begin
		c := 0;
		for v := 0 to n-1 do
			inc(c, (mask shr v) mod 2);

		if c = k then begin
			team := 0;
			for u := 0 to n-1 do
				if odd(mask shr u) then begin
					inc(team, a[u]);
					for v := u+1 to n-1 do
						if odd(mask shr v) then
							dec(team, issue[u, v]);
				end;
			ans := max(ans, team);
		end;
	end;
	writeln(ans);
end.
