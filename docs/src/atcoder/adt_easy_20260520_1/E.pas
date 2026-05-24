program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #sort #default #bisect
uses
	generics.collections,
	generics.defaults,
	math;
const
	NN = 300 * 1000;
var
	n, q, i, j, ai, b, l, r, m: int32;
	x: int64;
	a: tlist<int32>;
	s: array [0 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, q);

	a := tlist<int32>.create;
	for i := 0 to n-1 do begin
		read(ai);
		a.add(ai);
		a.exchange(i, random(i+1));
	end;
	readln;
	a.sort;

	s[0] := 0;
	for i := 0 to n-1 do s[i+1] := s[i] + a[i];

	for j := 1 to q do begin
		readln(b);

		if b > a[n-1] then
			x := -1
		else begin
			l := -1; // a[l] < b
			r := n-1; // a[r] >= b

			while r-l > 1 do begin
				m := (l+r) div 2;
				if a[m] < b then
					l := m
				else
					r := m;
			end;

			x := s[r] + int64(b-1) * (n-r) + 1;
		end;

		writeln(x);
	end;

	a.free;
end.
