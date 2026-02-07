program D_This_Is_the_Last_Time;
{$MODE DELPHI}
uses
	Generics.Defaults, Generics.Collections, sysutils, classes;
type
	TCasino = record
		l, r, k: int32;
	end;
	TCasinoComparer = class(TComparer<TCasino>)
		function Compare(constref Left, Right: TCasino): Integer; override;
	end;
var
	ntc, tci, n, k, i: int32;
	casino: TCasino;
	Comparer: TCasinoComparer;
	casinos: TList<TCasino>;

function TCasinoComparer.Compare(constref Left, Right: TCasino): Integer;
begin
	Result := Left.l - Right.l;
end;

begin
	randomize;
	Comparer := TCasinoComparer.Create;
	Comparer._AddRef;

	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n, k);

		casinos := TList<TCasino>.Create;
		for i := 0 to n-1 do begin
			readln(casino.l, casino.r, casino.k);
			casinos.Add(casino);
			casinos.Exchange(i, Random(i+1));
		end;
		casinos.Sort(Comparer);

		for i := 0 to n-1 do
			if (casinos[i].l <= k) and (k < casinos[i].k) then
				k := casinos[i].k;

		writeln(k);
		casinos.Free;

	end;

	Comparer._Release;
end.
