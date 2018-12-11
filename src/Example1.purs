      module Example1 where

      import Data.Lens
      import Data.Lens.At
      import Data.Profunctor.Strong
      import Data.Maybe
      import Data.Map as Map
      import Data.Map (Map)
      import Data.Tuple
      import Prelude

      type Category = String
      type SubCategory = String

      type Taxonomy = Tuple Category SubCategory

      stuffedAnimalsTaxonomy :: Taxonomy
      stuffedAnimalsTaxonomy = Tuple "toys & games" "stuffed animals"

      footballTaxonomy :: Taxonomy
      footballTaxonomy = Tuple "sports" "football"

      classicGamesTaxonomy :: Taxonomy
      classicGamesTaxonomy = Tuple "toys & games" "classic"


      viewCategory :: Taxonomy -> Category
      viewCategory (Tuple c s) = s

      viewSubcategory :: Taxonomy -> SubCategory
      viewSubcategory = snd

      setCategory :: Category -> Taxonomy -> Taxonomy
      setCategory newCat (Tuple _ s) = Tuple newCat s

      setSubcategory :: SubCategory -> Taxonomy -> Taxonomy
      setSubcategory newSubCat taxonomy = Tuple (fst taxonomy) newSubCat

      overCategory :: (Category -> Category) -> Taxonomy -> Taxonomy
      overCategory fn (Tuple c s) = Tuple (fn c) s

      overSubCategory :: (SubCategory -> SubCategory) -> Taxonomy -> Taxonomy
      overSubCategory fn (Tuple c s) = Tuple c (fn s)


      _categoryLens :: Lens' Taxonomy Category
      _categoryLens = lens getter setter
        where
          getter = fst
          setter (Tuple _ subcategory) newCategory = Tuple newCategory subcategory

      _subcategoryLens :: Lens' Taxonomy Category
      _subcategoryLens = _2

      _second :: forall a b ignored.
                 Lens (Tuple ignored a)
                      (Tuple ignored b)
                      a b 
      _second = _2


      type ToyID   = Int
      type Money   = Number

      type Toy = {
        id :: ToyID,
        name :: String,
        price :: Money,
        taxonomy :: Taxonomy
      }

      _priceLens :: Lens' Toy Number
      _priceLens = lens getter setter
        where
          getter = _.price
          setter toy newPrice = toy { price = newPrice }

      _nameLens :: Lens' Toy String
      _nameLens = lens getter setter
        where
          getter = _.name
          setter toy newName = toy { name = newName }

      _idLens :: forall s a b.
                 Lens { id :: a | s } -- Original whole (s)
                      { id :: b | s } -- New whole (t)
                      a b             -- original focus, new focus
      _idLens = lens _.id $ _ { id = _ }


      _toyTaxonomy :: Lens' Toy Taxonomy
      _toyTaxonomy = lens _.taxonomy $ _ { taxonomy = _ }

      _toyCategory :: Lens' Toy Category
      _toyCategory = _toyTaxonomy <<< _categoryLens

      teddyBear :: Toy
      teddyBear = { id: 1, name: "teddy bear", price: 25.2, taxonomy: stuffedAnimalsTaxonomy }

      football :: Toy
      football = { id: 2, name: "football", price: 18.0, taxonomy: footballTaxonomy }

      chess :: Toy
      chess = { id: 3, name: "chess", price: 45.0, taxonomy: classicGamesTaxonomy }

      -- set-get: view views what set sets

      setGet = set _toyCategory "new" football # view _toyCategory

      -- get-set: if you set what you get, nothing changes

      getSet = set _toyCategory (view _toyCategory football) football

      -- set-set multiple set == single set

      setSet = set _toyCategory "new" >>> set _toyCategory "new" $ football


      type Catalog = Map ToyID Toy

      _product1 :: Lens' Catalog (Maybe Toy)
      _product1 = lens getter setter
        where
          getter = Map.lookup 1
          setter cat wrapped =
            case wrapped of
              Just toy -> Map.insert 1 toy cat
              Nothing  -> Map.delete 1 cat


      _atKey :: forall key focus.
                Ord key => key -> 
                           Lens' (Map key focus) (Maybe focus)
      _atKey key = lens getter setter
        where
          getter = Map.lookup key
          setter whole wrapped =
            case wrapped of
              Just new -> Map.insert key new whole
              Nothing  -> Map.delete key whole

      -- at :: forall key value.
      --       Ord key => key -> 
      --                  Lens' (Map key value) (Maybe value)
      _product2 :: Lens' Catalog (Maybe Toy)
      _product2 = at 2


      type Store = {
        name :: String,
        catalog :: Catalog
      }

      _catalog :: Lens' Store Catalog
      _catalog = lens _.catalog $ _ { catalog = _ }

      _at1Toy :: Lens' Store (Maybe Toy)
      _at1Toy = _catalog <<< at 1

      _atnToy :: ToyID -> Lens' Store (Maybe Toy)
      _atnToy n = _catalog <<< at n



      {--set traversed 3 [1, 2] -- [3, 3]--}

      {--over traversed negate [1, 2] -- [-1, -2]--}
      {--over traversed negate (Just 3) -- (Just -3)--}

      {--view traversed ["g", "o", "o", "d"] -- "good"--}

      {--toListOf traversed [1, 2, 3] -- (1 : 2 : 3 : Nil)--}


      {--view traversed $ over traversed Additive [1, 2] -- Additive 3--}


      {--firstOf traversed [1, 2, 3] -- (Just 1)--}
      {--preview traversed [1, 2, 3] -- (Just 1)--}

      {--lastOf traversed [1, 2, 3] -- (Just 3)--}

      {--_element1 :: Traversal' (Array String) String--}
      {--_element1 = element 1 traversed--}

      {--over _element1 String.toUpper ["no", "yes!", "no"] -- ["no", "YES!", "no"]--}


      {--over (traversed <<< traversed) negate [ [1, 2], [3, 4]] -- [[-1,-2],[-3,-4]]--}

      discount :: Number -> Toy -> Toy
      discount percentage toy = set _priceLens (toy.price * percentage) toy
      discount percentage toy = toy { price = toy.price * percentage }

      {--over (at 1 <<< traversed) (discount 0.3) catalog--}
      {--view (at 1 <<< traversed <<< _nameLens) catalog--}

      {--ix 1 [1, 2, 3] -- 2--}

      over (ix 1) negate [1, 2, 3] -- [1, -2, 3]
      over (element 1 traversed) negate [1, 2, 3] -- [1, -2, 3]

-- over (at 1 <<< traversed <<< _priceLens) ((*)0.3) catalog

      _at1 = at 1
      _ix1 = ix 1

      m = Map.singleton 1 "a"

      view _at1 m    -- (Just "a")
      view _ix1 m    -- "a" (monoidal append)
      preview _ix1 m --(Just "a")

      
      set _at1 (Just "new value") m -- (fromFoldable [(Tuple 1 "new value")])

      set _ix1 "new value" m -- (fromFoldable [(Tuple 1 "new value")])

      set _ix1 "new value" Map.empty -- (fromFoldable [])

      preview _ix1 [1, 2] -- (Just 2)

      set _ix1 "ignored" [] -- []

      over _ix1 negate [0, 1] -- [0,-1]


  newtype Percent = Percent Number
  data Point      = Point Number Number

  data Fill 
    = Solid Color
    | LinearGradient Color Color Percent
    | RadialGradient Color Color Point
    | NoFill


  preview _solidFill $ Solid Color.white -- (Just rgba 255 255 255 1.0)
  preview _solidFill NoFill -- Nothing

  review _solidFill Color.white -- (Solid rgba 255 255 255 1.0)

  is _solidFill (Solid Color.white) :: Boolean -- true
  isn't _solidFill (Solid Color.white) :: Boolean -- false

  set _solidFill Color.white $ Solid Color.black -- (Solid rgba 255 255 255 1.0)
  set (_1 <<< _solidFill) Color.white $ Tuple (Solid Color.black) 5 
  -- (Tuple (Solid rgba 255 255 255 1.0) 5)



  _solidFill :: Prism' Fill Color
  _solidFill = prism' constructor focuser
    where
      constructor = Solid
      focuser fill = case fill of
        Solid color -> Just color
        _other      -> Nothing

  _solidWhite :: Prism' Fill Unit
  _solidWhite = only (Solid Color.white)

  -- is _solidWhite $ Solid Color.black :: Boolean

  _solidWhite' :: Prism' Fill Unit
  _solidWhite' = nearly (Solid Color.white) case _ of
    Solid color -> color == Color.white
    _ -> false

    Color.white # review _solidFill # preview _solidFill
    -- (Just Color.white)


    Solid Color.white # preview _solidFill <#> review _solidFill
    -- (Just (Solid Color.white))



  _centerPoint :: Prism' Fill Point
  centerPoint = prism' constructor focuser where
    focuser = case _ of
      RadialGradient _ _ point -> Just point
      _ -> Nothing
    constructor point = 
      RadialGradient Color.black Color.white point

  type RadialInterchange =
    { color1 :: Color
    , color2 :: Color
    , center :: Point
  }

  _centerPoint :: Prism' Fill RadialInterchange
  _centerPoint = prism constructor focuser
    where
      constructor {c1, c2, center} = 
        RadialGradient c1 c2 center
      focuser = case _ of
        RadialGradient c1 c2 center -> Right {c1, c2, center}
        otherCases ->                          Left otherCases

