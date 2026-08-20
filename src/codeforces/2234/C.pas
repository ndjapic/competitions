program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 3000;
var
	notc, tci, n, i, j: int32;
	ans: int64;
	h, w, w1, w2: array [0 .. 3*NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do read(h[i]);
		readln;

		for i := n+1 to 3*n do h[i] := h[i-n];

		for i := n+1 to 2*n do begin
			w1[i] := 0;
			for j := 1 to n-1 do
				w1[i+j] := max(w1[i+j-1], h[i+j-1]);

			w2[i] := 0;
			for j := 1 to n-1 do
				w2[i-j] := max(w2[i-j+1], h[i-j]);

			ans := 0;
			for j := 1 to n-1 do begin
				w[i] := min(w1[i+j], w2[i-n+j]);
				inc(ans, w[i]);
			end;

			write(ans);
			if i < 2*n then write(' ');
		end;
		writeln;

	end;
end.
