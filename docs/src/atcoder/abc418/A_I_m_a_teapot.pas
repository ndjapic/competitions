program A_I_m_a_teapot;
{$MODE DELPHI}
var
	n: int32;
	s: string;

begin
	readln(n);
	readln(s);

	if (n >= 3) and (s[n-2] = 't') and (s[n-1] = 'e') and (s[n] = 'a') then
		writeln('Yes')
	else
		writeln('No');
end.
