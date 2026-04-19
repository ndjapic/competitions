# Problem: D_Binary_String_Battle.pas

```pascal
program D_Binary_String_Battle;
{$MODE DELPHI}
var
	ntc, tci, n, k, i, c: int32;
	s: string;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n, k);
		readln(s);

		c := 0;
		for i := 1 to n do
			if s[i] = '1' then inc(c);

		if c <= k then
			writeln('Alice')
		else if 2*k <= n then
			writeln('Bob')
		else
			writeln('ALice');

	end;
end.

```
