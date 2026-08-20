program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	II = 4;
var
	n: int32;
	i: int8;
	d: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	setlength(d, II);
	for i := II downto 1 do begin
		d[i] := chr( ord('0') + n mod 10 );
		n := n div 10;
	end;

	writeln(d);
end.
