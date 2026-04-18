program C_Candy_Tribulation;
{$MODE DELPHI}
uses
	Generics.Defaults, Generics.Collections, math;
const
	nn = 200 * 1000;
var
	n, i, ai, x, y, g, p, q: int32;
	mn, s: int64;
	ans: boolean;
	a: TList<int32>;
	c: array [0 .. nn] of int64;

function gcd(a, b: int32): int32;
begin
    if b = 0 then
        gcd := a
    else
        gcd := gcd(b, a mod b);
end;

begin
	randomize;
	readln(n, x, y);
	g := gcd(x, y);
	p := x div g;
	q := (y-x) div g;

	a := TList<int32>.Create;
	for i := 0 to n-1 do begin
		read(ai);
		a.Add(ai);
		a.Exchange(i, random(i+1));
	end;
	readln;
	a.Sort;

	ans := true;
	c[n-1] := 0;
	mn := a[n-1] - c[n-1];
	s := 0;
	for i := n-2 downto 0 do
		if ans then begin
			ans := (a[i+1] - a[i]) mod q = 0;
			if ans then begin
				c[i] := c[i+1] + int64(a[i+1] - a[i]) div q * p;
				mn := min(mn, a[i] - c[i]);
				inc(s, c[i]);
			end;
		end;

	if (mn < 0) or not ans then
		writeln(-1)
	else
		writeln(s + mn * n);

	a.Free;
end.
