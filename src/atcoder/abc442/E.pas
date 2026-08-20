program E_;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections, math;
const
	nn = 200 * 1000;
type
	TMonsterComparer = class(TComparer<int32>)
		function Compare(constref l, r: int32): Integer; override;
	end;
var
	n, q, i, j, a, b, ans, l, r: int32;
	x, y: array [1 .. nn] of int64;
	rankl, rankr: array [1 .. nn] of int32;
	mon: TList<int32>;
	Comparer: TMonsterComparer;

function region(i: int32): int32; inline;
begin
	if (x[i] > 0) or (x[i] = 0) and (y[i] > 0) then
		Result := 1
	else
		Result := 2;
end;

function TMonsterComparer.Compare(constref l, r: int32): Integer;
begin
	Result := region(l) - region(r);
	if Result = 0 then Result := Sign(x[l] * y[r] - x[r] * y[l]);
end;

begin
	mon := TList<int32>.Create;
	Comparer := TMonsterComparer.Create;
	Comparer._AddRef;
	try

		readln(n, q);

		for i := 1 to n do begin
			readln(x[i], y[i]);
			mon.Add(i);
		end;
		mon.Sort(Comparer);

		l := 0;
		rankl[mon[l]] := l;
		for r := 1 to n-1 do begin
			if Comparer.Compare(mon[l], mon[r]) < 0 then l := r;
			rankl[mon[r]] := l;
		end;

		r := n-1;
		rankr[mon[r]] := r;
		for l := n-2 downto 0 do begin
			if Comparer.Compare(mon[l], mon[r]) < 0 then r := l;
			rankr[mon[l]] := r;
		end;

		for j := 1 to q do begin
			readln(a, b);
			ans := rankr[b] - rankl[a] + 1;
			if ans <= 0 then inc(ans, n);
			writeln(ans);
		end;

	finally
		mon.Free;
		Comparer._Release;
	end;
end.

(*
Time taken: 357 ms


------------------
(program exited with code: 0)
*)
