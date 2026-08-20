program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200 * 1000;
var
	n, i: int32;
	s, d: int64;
	l, r, a: array [1 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	s := 0;
	for i := 1 to n do begin
		readln(l[i], r[i]);
		if l[i] > 0 then
			a[i] := l[i]
		else if r[i] < 0 then
			a[i] := r[i]
		else
			a[i] := 0;
		inc(s, a[i]);
	end;

	for i := 1 to n do
		if s > 0 then begin
			d := min(s, a[i] - l[i]);
			dec(a[i], d);
			dec(s, d);
		end else if s < 0 then begin
			d := min(-s, r[i] - a[i]);
			inc(a[i], d);
			inc(s, d);
		end;

	if s = 0 then begin
		writeln('Yes');
		for i := 1 to n-1 do write(a[i], ' ');
		writeln(a[n]);
	end else
		writeln('No');
end.
