program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	h, w, q, i, r, c: int32;
	tp: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w, q);
	for i := 1 to q do begin
		read(tp);
		case tp of

			1: begin
				readln(r);
				writeln(r * w);
				dec(h, r);
			end;

			2: begin
				readln(c);
				writeln(c * h);
				dec(w, c);
			end;

		end;
	end;
end.
