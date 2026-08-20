program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #dp
uses
	math;
const
	NN = 200 * 1000;
var
	ntc, tci, n, i: int32;
	s: string;
	x, y: array [1 .. NN] of int32;
	sunny, rainy: array [1 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);
		readln(s);

		for i := 1 to n do read(x[i]); readln;
		for i := 1 to n-1 do read(y[i]); readln;

		case s[1] of

			'S': begin
				sunny[1] := 0;
				rainy[1] := -x[1];
			end;

			'R': begin
				sunny[1] := -x[1];
				rainy[1] := 0;
			end;

		end;

		for i := 1 to n-1 do
			case s[i+1] of

				'S': begin
					sunny[i+1] := max(sunny[i], rainy[i] + y[i]);
					rainy[i+1] := max(sunny[i], rainy[i]) - x[i+1];
				end;

				'R': begin
					sunny[i+1] := max(sunny[i], rainy[i] + y[i]) - x[i+1];
					rainy[i+1] := max(sunny[i], rainy[i]);
				end;

			end;

		writeln(max(sunny[n], rainy[n]));

	end;
end.
