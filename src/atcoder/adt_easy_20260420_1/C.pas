program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	n = 4;
var
	i, d: int8;
	ans0, ans1: boolean;
	x: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(x);

	ans0 := true;
	ans1 := true;

	for i := 1 to 3 do begin
		d := ord(x[i+1]) - ord(x[i]);
		if d < 0 then inc(d, 10);
		ans0 := ans0 and (d = 0);
		ans1 := ans1 and (d = 1);
	end;

	if ans0 or ans1 then
		writeln('Weak')
	else
		writeln('Strong');
end.
