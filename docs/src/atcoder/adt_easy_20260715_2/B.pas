program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	s: string;
	ans: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure next(var ch: char);
begin
	inc(ch);
	if ch = 'H' then ch := 'A';
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	ans := s[1] <= 'G';

	if ans then begin
		next(s[1]);
		next(s[1]);
		ans := s[1] = s[2];
	end;

	if ans then begin
		next(s[1]);
		next(s[1]);
		ans := s[1] = s[3];
	end;

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.
