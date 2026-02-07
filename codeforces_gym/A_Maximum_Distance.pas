program D_Stay_or_Mirror;
{$MODE DELPHI}{$INLINE ON}
uses
	math;
const
	nn = 5000;
type
	TPoint = class
		x, y: int32;
	end;
var
	n, i, j, ans: int32;
	a: array [1 .. nn] of TPoint;

function d2(a, b: TPoint): int32; inline;
begin
	Result := sqr(a.x - b.x) + sqr(a.y - b.y);
end;

begin
	readln(n);
	try

		for i := 1 to n do begin
			a[i] := TPoint.Create;
			read(a[i].x);
		end;
		readln;

		for i := 1 to n do begin
			read(a[i].y);
		end;
		readln;

		ans := 0;
		for i := 1 to n do
			for j := 1 to i-1 do
				ans := max(ans, d2(a[i], a[j]));

		writeln(ans);

	finally
		for i := 1 to n do a[i].Free;
	end;
end.
