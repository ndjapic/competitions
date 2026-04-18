program C_Reindeer_and_Sleigh_2;
{$MODE DELPHI}
uses
	Generics.Defaults, Generics.Collections, math;
const
	nn = 300 * 1000;
type
	TReindeer = record
		w, p: int64;
	end;
	TReindeerComparer = class(TComparer<TReindeer>)
		function Compare(constref Left, Right: TReindeer): Integer; override;
	end;
var
	notc, tci, n, i: int32;
	w, p: int64;
	ai: TReindeer;
	Comparer: TReindeerComparer;
	a: TList<TReindeer>;

function TReindeerComparer.Compare(constref Left, Right: TReindeer): Integer;
var
	d: int64;
begin
	d := -(Left.p + Left.w) + (Right.p + Right.w);
	if d < 0 then
		Result := -1
	else if d > 0 then
		Result := 1
	else
		Result := 0;
end;

begin
	randomize;
	readln(notc);
	for tci := 1 to notc do begin

		Comparer := TReindeerComparer.Create;
		Comparer._AddRef;
		a := TList<TReindeer>.Create;
		try

			readln(n);
			w := 0;
			p := 0;

			for i := 0 to n-1 do begin
				readln(ai.w, ai.p);
				inc(w, ai.w);
				a.Add(ai);
				a.Exchange(i, random(i+1));
			end;

			a.Sort(Comparer);

			i := 0;
			while (i < n) and (w > p) do begin
				dec(w, a[i].w);
				inc(p, a[i].p);
				inc(i);
			end;

			writeln(n-i);

		finally
			a.Free;
			Comparer._Release;
		end;

	end;
end.
