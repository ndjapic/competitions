program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, q, i, j, tp: int8;
	cards: array [1 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	for i := 1 to n do cards[i] := 0;

	for j := 1 to q do begin
		readln(tp, i);
		case tp of
			1: inc(cards[i]);
			2: inc(cards[i], 2);
			3: if cards[i] >= 2 then
				writeln('Yes')
			else
				writeln('No');
		end;
	end;
end.
