# Задатак: C_Dungeon.pas

```pascal
program C_Dungeon;
{$MODE DELPHI}
uses
	Generics.Defaults, Generics.Collections, math;
const
	nn = 200 * 1000;

type
	THeapComparer<_T> = class
		class function Compare(constref Left, Right: _T): SizeInt;
	end;
	THeap<_T, _C> = class
	private
		FItems: TList<_T>;
		function Favorite(u: SizeInt): SizeInt;
		function GetItem(Index: SizeInt): _T;
		function GetCount: SizeInt;
	public
		constructor Create;
		destructor Destroy; override;
		procedure Push(Item: _T);
		function Pop: _T;
		property Count: SizeInt read GetCount;
		property Items[Index: SizeInt]: _T read GetItem; default;
	end;

var
	notc, tci, n, m, i, j, k, sword, ans: int32;
	a, b, c, cpb, cpc: array [1 .. nn] of int32;
	pq: THeap<int32, THeapComparer<int32>>;

procedure MergeSort(lend, rend: int32);
var
	i, l, r, m: int32;
begin
	if rend - lend > 1 then begin
		m := (lend + rend) div 2;
		MergeSort(lend, m);
		MergeSort(m, rend);

		l := lend;
		r := m;
		for i := lend to rend - 1 do
			if (r = rend) or (l < m) and (
				(b[l] < b[r]) or
				(b[l] = b[r]) and
				(c[l] > c[r])
			) then begin
				cpb[i] := b[l];
				cpc[i] := c[l];
				inc(l);
			end else begin
				cpb[i] := b[r];
				cpc[i] := c[r];
				inc(r);
			end;

		for i := lend to rend - 1 do begin
			b[i] := cpb[i];
			c[i] := cpc[i];
		end;
	end;
end;

// THeapComparer<_T>
class function THeapComparer<_T>.Compare(constref Left, Right: _T): SizeInt;
begin
	Result := Left - Right;
end;

// THeap<_T, _C>
function THeap<_T, _C>.Favorite(u: SizeInt): SizeInt;
var
	v: SizeInt;
begin
	v := u * 2 + 2;
	if (v >= Count) or (_C.Compare(FItems[v-1], FItems[v]) < 0) then
		dec(v);
	Result := v;
end;

function THeap<_T, _C>.GetItem(Index: SizeInt): _T;
begin
	Result := FItems[Index];
end;

function THeap<_T, _C>.GetCount: SizeInt;
begin
	Result := FItems.Count;
end;

constructor THeap<_T, _C>.Create;
begin
	Inherited Create;
	FItems := TList<_T>.Create;
	{FItems.OwnsObjects := False;}
end;

destructor THeap<_T, _C>.Destroy;
begin
	FItems.Free;
	inherited Destroy;
end;

procedure THeap<_T, _C>.Push(Item: _T);
var
	u, v: SizeInt;
begin
	v := Count;
	FItems.Add(Item);
	u := (v-1) div 2;
	while (v > 0) and (_C.Compare(FItems[v], FItems[u]) < 0) do begin
		FItems.Exchange(u, v);
		v := u;
		u := (v-1) div 2;
	end;
end;

function THeap<_T, _C>.Pop: _T;
var
	u, v: SizeInt;
begin
	Result := FItems[0];
	FItems[0] := FItems[Count - 1];
	// If the Item itself needs to be freed, free it before deleting from the list
	FItems.Delete(Count - 1);

	u := 0;
	v := Favorite(u);
	while (v < Count) and (_C.Compare(FItems[v], FItems[u]) < 0) do begin
		FItems.Exchange(u, v);
		u := v;
		v := Favorite(u);
	end;
end;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, m);

		for i := 1 to n do read(a[i]); readln;
		for j := 1 to m do read(b[j]); readln;
		for j := 1 to m do read(c[j]); readln;
		MergeSort(1, m+1);

		ans := 0;
		pq := THeap<int32, THeapComparer<int32>>.Create;
		try

			for i := 1 to n do pq.Push(a[i]);
			k := 0;

			j := 1;
			while (j <= m) and (pq.Count > 0) do
				if c[j] = 0 then
					inc(j)
				else begin
					sword := pq.Pop;
					if sword < b[j] then begin
						inc(k);
						a[k] := sword;
					end else begin
						inc(ans);
						pq.Push(max(sword, c[j]));
						inc(j);
					end;
				end;

			for i := 1 to k do pq.Push(a[i]);

			j := 1;
			while (j <= m) and (pq.Count > 0) do
				if c[j] > 0 then
					inc(j)
				else begin
					sword := pq.Pop;
					if sword >= b[j] then begin
						inc(ans);
						inc(j);
					end;
				end;

		finally
			pq.Free;
		end;

		writeln(ans);

	end;
end.

```
