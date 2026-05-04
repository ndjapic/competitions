program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, m, i, j, c: int8;
	x: int32;
	a: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	c := 0;
	for j := 1 to m do begin
		read(a[j]);
		if a[j] <= 0 then inc(c);
	end;
	readln;

	for i := 1 to n do begin
		for j := 1 to m do begin
			read(x);
			if a[j] > 0 then begin
				dec(a[j], x);
				if a[j] <= 0 then inc(c);
			end;
		end;
		readln;
	end;

	if c = m then
		writeln('Yes')
	else
		writeln('No');
end.
