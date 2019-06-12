--1a)

--TAD (Tipo Abstracto de Datos)
type Recurso = String

data Pais = UnPais {
  ingresoPerCapita :: Float,
  sectorPublico :: Int,
  sectorPrivado :: Int,
  recursosNaturales :: [Recurso],
  deudaFMI :: Float
} deriving Show

--1b)
namibia = UnPais 4140 400000 650000 ["Minería", "Ecoturismo"] 50

--2)
type Estrategia = Pais -> Pais

prestar :: Float -> Estrategia
prestar millones pais = pais {deudaFMI = deudaFMI pais + millones}

reducirPuestoPublico :: Int -> Estrategia
reducirPuestoPublico puestos pais =
  disminuirIPC pais {sectorPublico = sectorPublico pais - puestos}

disminuirIPC :: Pais -> Pais
disminuirIPC pais
  | ingresoPerCapita pais > 100 = 
    pais {ingresoPerCapita = ingresoPerCapita pais * 0.8}
  | otherwise =
    pais {ingresoPerCapita = ingresoPerCapita pais * 0.85}

explotacion :: Recurso -> Estrategia
explotacion recurso pais = 
  pais {recursosNaturales = quitarRecurso recurso (recursosNaturales pais),
        deudaFMI = deudaFMI pais - 2}

quitarRecurso :: Recurso -> [Recurso] -> [Recurso]
quitarRecurso recurso recursos = filter (/= recurso) recursos

blindaje :: Estrategia
blindaje pais= 
  reducirPuestoPublico 500.prestar (productoBrutoInterno pais /2)
  $ pais

productoBrutoInterno :: Pais -> Float
productoBrutoInterno pais = 
  ingresoPerCapita pais * fromIntegral (poblacionActiva pais)

poblacionActiva :: Pais -> Int
poblacionActiva pais = sectorPrivado pais + sectorPublico pais

--3a)
type Receta = [Estrategia]

receta1 = [prestar 200, explotacion "Minería"]

--3b)
aplicarReceta :: Receta -> Pais -> Pais
aplicarReceta receta pais = 
  foldr ($) pais receta

--4a)
losQueZafan :: [Pais] -> [Pais]
losQueZafan = filter (elem "Petróleo" . recursosNaturales)

--4b)
totalDeudas :: [Pais] -> Float
totalDeudas = sum . map deudaFMI

--5)
estaOrdenado :: Pais -> [Receta] -> Bool
estaOrdenado _ [receta] = True
estaOrdenado pais (receta1:receta2:recetas)
  | revisarPBI receta1 pais < revisarPBI receta2 pais 
    && estaOrdenado pais (receta2:recetas) = True
  | otherwise = False

  where
    revisarPBI receta = productoBrutoInterno . aplicarReceta receta
