program sorted_dict_fgl;
{$MODE OBJFPC} // Препоручено за fgl генерике
uses
	fgl; // Ова библиотека ради на СВИМ такмичарским платформама

type
	// Дефинишемо структуру која је директна замена за ваш AVL приступ
	generic TSortedDictionary<TKey, TValue> = class
	public
		type
			// Компатибилност са вашим Понтер/Рекорд типом за излаз
			PNodeData = ^TNodeData;
			TNodeData = record
				Key: TKey;
				Value: TValue;
			end;
	private
		type
			// Правимо интерну мапу засновану на уграђеним fgl генерицима
			TInternalMap = specialize TFPGMap<TKey, TValue>;
		var
			FMap: TInternalMap;
			FTempNode: TNodeData; // Помоћни простор за враћање показивача
	public
		constructor Create;
		procedure Clear;
		destructor Destroy; override;
		procedure Add(const K: TKey; const V: TValue);
		function TryGetValue(const K: TKey; out V: TValue): Boolean;
		function FindLowest: PNodeData;
		function FindHighest: PNodeData;
		procedure Remove(const K: TKey);
		function Count: Integer;
	end;

constructor TSortedDictionary.Create;
begin
	FMap := TInternalMap.Create;
	// fgl аутоматски сортира кључеве по њиховој природној вредности
end;

procedure TSortedDictionary.Clear;
begin
	FMap.Clear;
end;

destructor TSortedDictionary.Destroy;
begin
	FMap.Free;
	inherited;
end;

procedure TSortedDictionary.Add(const K: TKey; const V: TValue);
var
	Idx: Integer;
begin
	// Ако кључ већ постоји, fgl дозвољава измену или додавање
	if FMap.Find(K, Idx) then
		FMap.Data[Idx] := V
	else
		FMap.Add(K, V);
end;

function TSortedDictionary.TryGetValue(const K: TKey; out V: TValue): Boolean;
var
	Idx: Integer;
begin
	Result := FMap.Find(K, Idx);
	if Result then
		V := FMap.Data[Idx];
end;

function TSortedDictionary.FindLowest: PNodeData;
begin
	if FMap.Count = 0 then Exit(nil);
	FTempNode.Key := FMap.Keys[0];
	FTempNode.Value := FMap.Data[0];
	Result := @FTempNode;
end;

function TSortedDictionary.FindHighest: PNodeData;
begin
	if FMap.Count = 0 then Exit(nil);
	FTempNode.Key := FMap.Keys[FMap.Count - 1];
	FTempNode.Value := FMap.Data[FMap.Count - 1];
	Result := @FTempNode;
end;

procedure TSortedDictionary.Remove(const K: TKey);
begin
	FMap.Remove(K);
end;

function TSortedDictionary.Count: Integer;
begin
	Result := FMap.Count;
end;

// --- Пример покретања (тест за такмичења) ---
type
	TMyDict = specialize TSortedDictionary<LongInt, string>;
var
	Dict: TMyDict;
	Lowest, Highest: TMyDict.PNodeData;
begin
	Dict := TMyDict.Create;
	
	Dict.Add(50, 'Педесет');
	Dict.Add(10, 'Десет');
	Dict.Add(90, 'Деведесет');
	
	Lowest := Dict.FindLowest;
	Highest := Dict.FindHighest;
	
	WriteLn('Најмањи кључ: ', Lowest^.Key, ' -> ', Lowest^.Value);	 // Исписује 10
	WriteLn('Највећи кључ: ', Highest^.Key, ' -> ', Highest^.Value); // Исписује 90
	
	Dict.Free;
end.
