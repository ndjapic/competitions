# Задатак: B_Impost_or_Sus.pas

```pascal
program B_Impost_or_Sus;
var
	notc, tci, n, l, r, ans: int32;
	s: string;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(s);
		n := length(s);

		ans := 0;

		if s[1] = 'u' then begin
			s[1] := 's';
			inc(ans);
		end;

		if s[n] = 'u' then begin
			s[n] := 's';
			inc(ans);
		end;

		l := 1;
		for r := 1 to n do
			if s[r] = 's' then begin
				inc(ans, (r-l) div 2);
				l := r+1;
			end;

		writeln(ans);

	end;
end.

```
