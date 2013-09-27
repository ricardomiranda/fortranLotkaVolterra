!------------------------------------------------------------------------------
!        IST/MARETEC, Water Modelling Group, Mohid modelling system
!------------------------------------------------------------------------------
!
! TITLE         : Lotka Volterra
! DATE          : July 2013
! REVISION      : Ricardo Miranda
! DESCRIPTION   : Example program of Functional Core, Imperative LotkaVolterra according to
!                 Gary Bernhardt
!
!------------------------------------------------------------------------------
!
!This program is free software; you can redistribute it and/or
!modify it under the terms of the GNU General Public License
!version 2, as published by the Free Software Foundation.
!
!This program is distributed in the hope that it will be useful,
!but WITHOUT ANY WARRANTY; without even the implied warranty of
!MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!GNU General Public License for more details.
!
!You should have received a copy of the GNU General Public License
!along with this program; if not, write to the Free Software
!Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
!
!------------------------------------------------------------------------------

program MohidLotkaVolterra

    use ModulePrey
    use ModulePredators

    implicit none

    type T_LV
        real(8)                     :: DT, NbrSteps
    end type T_LV

    call ModifyLotkaVolterra

    contains

    !--------------------------------------------------------------------------

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONS

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    subroutine ConstructMohidLotkaVolterra (ObjLotkaVolterra)

        !Arguments---------------------------------------------------------------
        type(T_LV), pointer :: ObjLotkaVolterra

        !------------------------------------------------------------------------

        call AllocateInstance (ObjLotkaVolterra)
        call ASkQuestions     (ObjLotkaVolterra)

    end subroutine ConstructMohidLotkaVolterra

    !--------------------------------------------------------------------------

    subroutine ASkQuestions (ObjLotkaVolterra)

        !Arguments---------------------------------------------------------------
        type(T_LV), pointer :: ObjLotkaVolterra

        !Local-------------------------------------------------------------------

        !------------------------------------------------------------------------

        print*, "What is the time step?"
        read*,  ObjLotkaVolterra%DT

        print*, "How many time steps to compute?"
        read*,  ObjLotkaVolterra%NbrSteps

    end subroutine ASkQuestions

    !--------------------------------------------------------------------------

    function StratPrey ()

        !Local-------------------------------------------------------------------
        real(8) :: IniPreyPopulation

        real(8) :: PreyBirthRate
        real(8) :: PreyDestroyRate

        !Return----------------------------------------------------------------
        type (T_Prey), pointer                          :: StratPrey

        !----------------------------------------------------------------------

        print*, "What is the incial prey population?"
        read*,  IniPreyPopulation

        print*, "What is the prey birth rate?"
        read*,  PreyBirthRate

        print*, "What is the prey destroy rate?"
        read*,  PreyDestroyRate

        StratPrey => ConstructPrey (IniPreyPopulation, PreyBirthRate, PreyDestroyRate)

    end function StratPrey

    !--------------------------------------------------------------------------

    function StratPredators ()

        !Local-----------------------------------------------------------------
        real(8) :: IniPredatorsPopulation

        real(8) :: PredatorsIncreaseRate
        real(8) :: PredatorsDeathRate

        !Return----------------------------------------------------------------
        type (T_Predators), pointer                          :: StratPredators

        !----------------------------------------------------------------------

        print*, "What is the incial predators population?"
        read*,  IniPredatorsPopulation

        print*, "What is the predators increase rate?"
        read*,  PredatorsIncreaseRate

        print*, "What is the predators death rate?"
        read*,  PredatorsDeathRate

        StratPredators => ConstructPredators (IniPredatorsPopulation, PredatorsIncreaseRate, PredatorsDeathRate)

    end function StratPredators

    !--------------------------------------------------------------------------

    pure subroutine AllocateInstance (ObjLotkaVolterra)
        !Arguments-------------------------------------------------------------
        type(T_LV), pointer :: ObjLotkaVolterra

        !----------------------------------------------------------------------

        allocate (ObjLotkaVolterra)

    end subroutine AllocateInstance

    !--------------------------------------------------------------------------

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODI

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !--------------------------------------------------------------------------

    subroutine ModifyLotkaVolterra

        !Local-----------------------------------------------------------------
        type(T_LV),        pointer :: ObjLotkaVolterra
        type(T_Prey),      pointer :: ObjPrey
        type(T_Predators), pointer :: ObjPredators

        !----------------------------------------------------------------------

        call ConstructMohidLotkaVolterra (ObjLotkaVolterra)

        ObjPrey      => StratPrey        ()
        ObjPredators => StratPredators   ()
        call Loop                        (ObjLotkaVolterra, ObjPrey, ObjPredators, ObjLotkaVolterra%NbrSteps)

    end subroutine ModifyLotkaVolterra

    !--------------------------------------------------------------------------

    recursive subroutine Loop (ObjLotkaVolterra, ObjPrey, ObjPredators, NbrSteps)

        !Arguments-------------------------------------------------------------
        type(T_LV),        pointer :: ObjLotkaVolterra
        type(T_Prey),      pointer :: ObjPrey
        type(T_Predators), pointer :: ObjPredators
        real(8), intent(IN)        :: NbrSteps

        !Local-----------------------------------------------------------------
        type(T_Prey), pointer      :: NewObjPrey
        type(T_Predators), pointer :: NewObjPredators

        real(8)                    :: PreyPopulationSize
        real(8)                    :: PredatorsPopulationSize

        !----------------------------------------------------------------------

cd1 :   if (NbrSteps .LE. 0.0) then
            print*, "Simulation terminated successfully."

            call KillLotkaVolterra (ObjLotkaVolterra, ObjPrey, ObjPredators)

        else   cd1
            PreyPopulationSize      = GetPreyPopulationSize      (ObjPrey)
            PredatorsPopulationSize = GetPredatorsPopulationSize (ObjPredators)

            print*, NbrSteps, PreyPopulationSize, PredatorsPopulationSize

            NewObjPrey      => ModifyPreyPopulation      (ObjPrey,      PredatorsPopulationSize, ObjLotkaVolterra%DT)
            call PreyGarbageCollector                    (ObjPrey)

            NewObjPredators => ModifyPredatorsPopulation (ObjPredators, PreyPopulationSize,      ObjLotkaVolterra%DT)
            call PredatorsGarbageCollector               (ObjPredators)

            call Loop (ObjLotkaVolterra, NewObjPrey, NewObjPredators, NbrSteps - ObjLotkaVolterra%DT)
        end if cd1

    end subroutine Loop

    !--------------------------------------------------------------------------

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



    pure subroutine KillLotkaVolterra (ObjLotkaVolterra, ObjPrey, ObjPredators)
        !Arguments-------------------------------------------------------------
        type(T_LV),        pointer :: ObjLotkaVolterra
        type(T_Prey),      pointer :: ObjPrey
        type(T_Predators), pointer :: ObjPredators

        !----------------------------------------------------------------------

        call KillPrey      (ObjPrey)
        call KillPredators (ObjPredators)

        deallocate         (ObjLotkaVolterra)

    end subroutine KillLotkaVolterra

    !--------------------------------------------------------------------------

end program MohidLotkaVolterra
