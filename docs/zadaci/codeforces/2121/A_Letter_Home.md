# Задатак: A_Letter_Home.pas

```pascal
program A_Letter_Home;
uses
	math;
var
    ntc, tci, n, s, i, x, l, r, ans: int16;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, s);

		l := 100;
		r := 1;

		for i := 1 to n do begin
			read(x);
			l := min(l, x);
			r := max(r, x);
		end;
		readln;

		if s < l then
			ans := r-s
		else if s > r then
			ans := s-l
		else
			ans := min(s-l, r-s) + r-l;

		writeln(ans);

    end;
end.

```
