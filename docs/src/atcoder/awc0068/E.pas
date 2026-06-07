program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #unsolved #max #sum #sliding_window #deque
uses
	Math;
const
	nn = 1000;
var
	n, k, i, j, l, r: int32;
	a, mx1: array [1 .. nn, 1 .. nn] of int32;
	s1: array [0 .. nn, 0 .. nn] of int64;
	dq: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	for i := 1 to n do begin
		s1[i, 0] := 0;
		l := 1;
		r := 0;

		for j := 1 to n do begin
			read(a[i, j]);

			s1[i, j] := s1[i, j-1] + a[i, j];
			if j-k > 0 then dec(s1[i, j], a[i, j-k]);

			if (l <= r) and (dq[l] = j-k) then inc(l);
			while (l <= r) and (a[i, dq[r]] <= a[i, j]) do dec(r);

			inc(r);
			dq[r] := j;

			mx1[i, j] := a[i, dq[l]];
		end;
		readln;
	end;

	for j := 1 to n do begin
		s1[0, j] := 0;
	end;

end.
