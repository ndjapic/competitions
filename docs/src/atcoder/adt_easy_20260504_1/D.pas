program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 10;
var
	n, i, j: int8;
	a: array [1 .. nn] of int8;
	seen: array [1 .. nn] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for j := 1 to n do seen[j] := false;

	for i := 1 to n do begin
		read(j);
		a[i] := j;
		seen[j] := true;
	end;
	readln;

	j := 1;
	for i := 1 to n do
		if a[i] = -1 then begin
			while seen[j] do inc(j);
			a[i] := j;
			seen[j] := true;
		end;

	j := 1;
	while (j <= n) and seen[j] do inc(j);

	if j <= n then
		writeln('No')
	else begin
		writeln('Yes');
		for i := 1 to n-1 do write(a[i], ' ');
		writeln(a[n]);
	end;
end.
