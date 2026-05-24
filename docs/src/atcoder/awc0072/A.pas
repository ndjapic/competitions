program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int32;
	s, t: string;
	contra: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);
	readln(t);

	contra := false;
	for i := 1 to n do
		if (s[i] = '?') and (t[i] = '?') then
		else if t[i] = '?' then
		else if s[i] = '?' then
			s[i] := t[i]
		else if s[i] <> t[i] then begin
			s[i] := '!';
			contra := true;
		end;

	writeln(s);
	if contra then
		writeln('Yes')
	else
		writeln('No');
end.
