program B_Get_Min;
{$MODE DELPHI}
// #heap
uses
	Generics.Collections;

type
	TMyComparer<_T> = class
		function Compare(constref Left, Right: _T): Integer;
	end;
	THeap<_T> = class
	public
		Items: TList<_T>;
		constructor Create;
		destructor Destroy; override;
		procedure Push(Item: _T);
		function Favorite(u: SizeInt): SizeInt;
		function Pop: _T;
	end;

var
	q, x, i, tp: int8;
	Comparer: TMyComparer<int8>;
	pq: THeap<int8>;

function TMyComparer<_T>.Compare(constref Left, Right: _T): Integer;
begin
	Result := Left - Right;
end;

constructor THeap<_T>.Create;
begin
	Inherited Create;
	Items := TList<_T>.Create;
end;

destructor THeap<_T>.Destroy;
begin
	// Perform cleanup specific to THeap here
	Items.Free;
	inherited Destroy; // Call the parent destructor
end;

procedure THeap<_T>.Push(Item: _T);
var
	u, v: SizeInt;
begin
	v := Items.Count;
	Items.Add(Item);
	u := (v-1) div 2;
	while (v > 0) and (Comparer.Compare(Items[v], Items[u]) < 0) do begin
		Items.Exchange(u, v);
		v := u;
		u := (v-1) div 2;
	end;
end;

function THeap<_T>.Favorite(u: SizeInt): SizeInt;
var
	v: SizeInt;
begin
	v := u * 2 + 2;
	if (v >= Items.Count) or (Comparer.Compare(Items[v], Items[v-1]) >= 0) then
		dec(v);
	Result := v;
end;

function THeap<_T>.Pop: _T;
var
	u, v: SizeInt;
begin
	Result := Items[0]; // Get the first element
	Items[0] := Items[Items.Count - 1];
	// If the Item itself needs to be freed, free it before deleting from the list
	// Items[Items.Count - 1].Free;
	// FreeAndNil(Items[Items.Count - 1]);
	Items.Delete(Items.Count - 1); // Remove the last element
	u := 0;
	v := Favorite(u);
	while (v < Items.Count) and (Comparer.Compare(Items[v], Items[u]) < 0) do begin
		Items.Exchange(u, v);
		u := v;
		v := Favorite(u);
	end;
end;

begin
	Comparer := TMyComparer<int8>.Create;
	pq := THeap<int8>.Create;
	try

		readln(q);

		for i := 1 to q do begin
			read(tp);
			case tp of

				1: begin
					read(x);
					pq.Push(x);
				end;

				2: writeln(pq.Pop);

			end;
			readln;
		end;

	finally
		pq.Free;
		Comparer.Free;
	end;
end.
