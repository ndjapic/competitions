program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #sum #limits
uses
	math;
const
	NN = 200 * 1000;
var
	n, i: int32;
	s, d: int64;
	l, r, x: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	s := 0;
	for i := 1 to n do begin
		readln(l[i], r[i]);
		inc(s, r[i]);
	end;

	if s >= 0 then
		for i := 1 to n do begin
			d := min(s, r[i] - l[i]);
			x[i] := r[i] - d;
			dec(s, d);
		end;

	if s <> 0 then
		writeln('No')
	else begin
		writeln('Yes');
		for i := 1 to n-1 do write(x[i], ' ');
		writeln(x[n]);
	end;
end.
