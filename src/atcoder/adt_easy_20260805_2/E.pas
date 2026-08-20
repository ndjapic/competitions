program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 200 * 1000;
var
	n, i, d, m, k: int32;
	s, ans: int64;
	a: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	s := 0;
	for i := 1 to n do begin
		read(a[i]);
		inc(s, a[i]);
	end;
	readln;

	d := s div n;
	m := s mod n;
	k := n-m;

	ans := 0;
	for i := 1 to n do
		if a[i] > d then begin

			if m > 0 then begin
				inc(ans, a[i] - (d+1));
				dec(m);
			end else
				inc(ans, a[i] - d);

		end else begin

			if k > 0 then begin
				inc(ans, d - a[i]);
				dec(k);
			end else
				inc(ans, (d+1) - a[i]);

		end;

	writeln(ans div 2);
end.
