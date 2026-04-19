# Problem: A_Capitalized.pas

```pascal
program A_Capitalized;
{$H+}
const
    maxn = 200 * 1000;
var
    n, i: int32;
    ans: boolean;
    s: string;

begin
    readln(s);
    n := length(s);

	ans := (s[1] >= 'A') and (s[1] <= 'Z');

	if ans then begin
		i := 2;
		while (s[i] >= 'a') and (s[i] <= 'z') do inc(i);
		ans := i > n;
	end;

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.

```
