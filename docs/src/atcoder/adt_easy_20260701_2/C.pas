program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #stack
uses
	generics.collections;
const
	NN = 100;
var
	q, i, tp, x: int8;
	s: tstack<int8>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(q);

	s := tstack<int8>.create;
	for i := 1 to NN do s.push(0);

	for i := 1 to q do begin
		read(tp);
		case tp of

			1: begin
				read(x);
				s.push(x);
			end;

			2: writeln(s.pop);

		end;
		readln;
	end;
end.
