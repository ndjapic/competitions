program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, s, i: int8;
	t0, t1: int32;
	ans: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, s);

	t0 := 0;
	ans := true;
	for i := 1 to n do begin
		read(t1);
		if ans and (t1 - t0 > s) then ans := false;
		t0 := t1;
	end;
	readln;

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.
