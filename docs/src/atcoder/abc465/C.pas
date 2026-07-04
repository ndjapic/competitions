program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #naive #TLE
const
	NN = 500 * 1000;
var
	n, i, l, r, x: int32;
	s: string;
	a: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);

	for i := 1 to n do a[i] := i;

	for i := 1 to n do
		if s[i] = 'o' then begin
			l := 1;
			r := i;
			while l < r do begin
				x := a[l];
				a[l] := a[r];
				a[r] := x;
				inc(l);
				dec(r);
			end;
		end;

	for i := 1 to n-1 do write(a[i], ' ');
	writeln(a[n]);
end.
