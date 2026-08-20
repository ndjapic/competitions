program A_Weak_Beats;
uses
	math;
const
    maxn = 16;
var
    n, i: int32;
    ans: boolean;
    s: array [1 .. maxn] of char;

begin
	n := 16;
	ans := true;
	for i := 1 to n do begin
		read(s[i]);
		if not odd(i) then
			ans := ans and (s[i] = '0');
	end;
	readln;

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.
