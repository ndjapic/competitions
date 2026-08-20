program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	s: string;
	ans: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	ans := (s = 'ACE') or (s = 'BDF') or (s = 'CEG')
		or (s = 'DFA') or (s = 'EGB') or (s = 'FAC')
		or (s = 'GBD');

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.
