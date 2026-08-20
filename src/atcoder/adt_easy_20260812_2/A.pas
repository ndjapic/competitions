program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	SysUtils;
var
	i, j, a, b: int8;
	ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	ans := 1;

	for i := 1 to 9 do begin
		read(a);
		inc(ans, a);
	end;

	for j := 1 to 8 do begin
		read(b);
		dec(ans, b);
	end;

	writeln(ans);
end.
