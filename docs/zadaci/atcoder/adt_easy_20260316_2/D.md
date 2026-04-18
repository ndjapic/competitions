# Задатак: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	top, q, i, tp, x: int32;
	stack: array [1 .. 2*nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	for top := 1 to 100 do stack[top] := 0;
	top := 100;

	readln(q);

	for i := 1 to q do begin
		read(tp);
		case tp of

			1: begin
				read(x);
				inc(top);
				stack[top] := x;
			end;

			2: begin
				writeln(stack[top]);
				dec(top);
			end;

		end;
		readln;
	end;
end.

```
