program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure spells(x: int64);
begin
	if x = 0 then
		writeln
	else if odd(x) then begin
		spells(x - 1);
		write('A');
	end else begin
		spells(x div 2);
		write('B');
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	spells(n);
end.
