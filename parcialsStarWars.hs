

data Nave = Nave {
durabilidad :: Int,
escudo :: Int,
ataque :: Int,
poder :: [Poderes]
}

type Poderes = Nave -> Nave 

-- PUNTO 1 MODELADO DE NAVES 

tiefighter = Nave {
    durabilidad = 200,
    escudo = 100,
    ataque = 50,
    poder = [movimientoTurbo 1]
}

xWing = Nave {
durabilidad = 300,
escudo = 150,
ataque = 100,
poder = [reparacionDeEmergencia]
}

naveDeDarthVader = Nave {
durabilidad = 500,
escudo = 500,
ataque = 50,
poder = [movimientoTurbo 3, mapDurabilidad (subtract 45)]
}

milleniumFalcon = Nave {
durabilidad = 1000,
escudo = 500,
ataque = 50,
poder = [reparacionDeEmergencia, mapEscudos (+100)]
}

naveNueva = Nave {
durabilidad = 100,
escudo = 50,
ataque = 1000,
poder = [reparacionDeEmergencia, movimientoTurbo 5]    
}



-- FUNCIONES PARA LOS PODERES. 

movimientoTurbo :: Int -> Nave -> Nave
movimientoTurbo  cantidad = mapAtaque ( (cantidad * 25) +)

movimientoSuperTurbo :: Nave -> Nave 
movimientoSuperTurbo = mapAtaque (+75) . mapDurabilidad (subtract 45) 



mapAtaque :: (Int -> Int) -> Nave -> Nave
mapAtaque f unaNave = unaNave {ataque = f $ ataque unaNave } 

reparacionDeEmergencia :: Nave -> Nave 
reparacionDeEmergencia = mapDurabilidad (+50). mapAtaque (subtract 30)

mapDurabilidad :: (Int -> Int) -> Nave -> Nave 
mapDurabilidad f unaNave = unaNave {durabilidad = f $ durabilidad unaNave} 

mapEscudos :: (Int -> Int) -> Nave -> Nave 
mapEscudos f unaNave = unaNave {escudo = f $ escudo unaNave }

--PUNTO 2 
durabilidadUnaFlota :: [Nave] -> Int
durabilidadUnaFlota  = sumatoriaDeDurabilidades 

sumatoriaDeDurabilidades :: [Nave] -> Int 
sumatoriaDeDurabilidades = sum . map durabilidad 


estadoPostAtaque :: Nave -> Nave -> Nave 
estadoPostAtaque naveAtacante naveAtacada = calculoDeEstado (utilizarPoderes naveAtacante) (utilizarPoderes naveAtacada)

calculoDeEstado :: Nave -> Nave -> Nave 
calculoDeEstado naveAtacante naveAtacada
    | escudo naveAtacada > ataque naveAtacante = naveAtacada
    |otherwise = mapDurabilidad (subtract (danoRecibido naveAtacante naveAtacada)) naveAtacada

danoRecibido :: Nave -> Nave -> Int
danoRecibido naveAtacante naveAtacada = ataque naveAtacante - escudo naveAtacada

utilizarPoderes :: Nave -> Nave 
utilizarPoderes unaNave = foldl (\ x f -> f x) unaNave (poder unaNave)

--PUNTO 4
fueraDeCombate :: Nave -> Bool
fueraDeCombate = (==0) . durabilidad 


--PUNTO 5 
type Estrategia = Nave -> Bool

navesDebil :: Nave -> Bool
navesDebil = (<200).escudo 

naveConPeligrosidad :: Int -> Nave -> Bool 
naveConPeligrosidad cotaDeAtaque = (>cotaDeAtaque) . ataque  

naveFueraDeCombate :: Nave -> Nave -> Bool 
naveFueraDeCombate naveAtacante = (==0) . durabilidad. estadoPostAtaque naveAtacante

naveFundida :: Nave -> Bool
naveFundida  unaNave = ((<50) .escudo) unaNave && ((<70) . ataque) unaNave 


estadoFlotaEnemiga :: [Nave] -> Nave -> Estrategia -> [Nave]
estadoFlotaEnemiga flotaEnemiga unaNave unaEstrategia = (danoAlaFlota unaNave . determinarObjetivos unaEstrategia) flotaEnemiga 

danoAlaFlota :: Nave -> [Nave] -> [Nave]
danoAlaFlota unaNave = map (estadoPostAtaque unaNave) 

determinarObjetivos :: Estrategia -> [Nave] -> [Nave]
determinarObjetivos = filter 
