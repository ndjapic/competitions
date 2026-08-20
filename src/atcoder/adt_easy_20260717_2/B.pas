program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, l, r: int8;
	space: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure wrt(i: int8);
begin
	if space then
		write(' ')
	else
		space := true;
	write(i);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, l, r);

	space := false;

	for i := 1 to l-1 do wrt(i);
	for i := r downto l do wrt(i);
	for i := r+1 to n do wrt(i);
	writeln;
end.
