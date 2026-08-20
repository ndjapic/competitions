program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, i: int8;
	a: array [1 .. nn] of int8;
	found: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	found := false;
	read(a[1], a[2]);

	for i := 3 to n do begin
		read(a[i]);
		if not found then
			found := (a[i-2] = a[i]) and (a[i-1] = a[i]);
	end;

	if found then
		writeln('Yes')
	else
		writeln('No');
end.
