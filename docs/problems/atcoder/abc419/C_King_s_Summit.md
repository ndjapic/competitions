# Problem: C_King_s_Summit.pas

```pascal
program C_King_s_Summit;
uses
	math;
const
	inf = 1000 * 1000 * 1000;
var
	n, k, li, lj, ri, rj, r, c, mx: int32;
begin
	readln(n);
	li := inf;
	lj := inf;
	ri := 1;
	rj := 1;

	for k := 1 to n do begin
		readln(r, c);
		li := min(li, r);
		lj := min(lj, c);
		ri := max(ri, r);
		rj := max(rj, c);
	end;

	mx := max(ri-li, rj-lj);
	writeln((mx + 1) div 2);
end.

```
