program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	h, w, q, i, tp, x: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w, q);

	for i := 1 to q do begin
		readln(tp, x);
		case tp of

			1: begin
				writeln(x * w);
				dec(h, x);
			end;

			2: begin
				writeln(x * h);
				dec(w, x);
			end;

		end;
	end;
end.
