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
        type(T_Prey), pointer       :: ObjPrey
        type(T_Predators), pointer  :: ObjPredators

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

    subroutine AllocateInstance (ObjLotkaVolterra)
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

        call ConstructMohidLotkaVolterra (                           ObjLotkaVolterra)

        ObjPrey      => StratPrey      ()
        ObjPredators => StratPredators ()
        call Loop                        (ObjPrey, ObjPredators, ObjLotkaVolterra%DT, ObjLotkaVolterra%NbrSteps)

        call KillLotkaVolterra           (                       ObjLotkaVolterra)

    end subroutine ModifyLotkaVolterra
    
    !--------------------------------------------------------------------------

!    recursive subroutine Loop (ObjLotkaVolterra%ObjPrey, ObjLotkaVolterra%ObjPredators, NbrSteps)
    recursive subroutine Loop (ObjPrey, ObjPredators, DT, NbrSteps)
        
        !Arguments-------------------------------------------------------------         
        type(T_Prey),      pointer  :: ObjPrey
        type(T_Predators), pointer  :: ObjPredators
        real(8), intent(IN)         :: DT
        real(8), intent(IN)         :: NbrSteps

        !Local-----------------------------------------------------------------
        type(T_Prey), pointer       :: NewObjPrey
        type(T_Predators), pointer  :: NewObjPredators

        real(8)                     :: PreyPopulationSize
        real(8)                     :: PredatorsPopulationSize

        !----------------------------------------------------------------------

cd1 :   if (NbrSteps .LE. 0.0) then
            print*, "Simulation terminated successfully."

        else   cd1
            PreyPopulationSize      = GetPreyPopulationSize      (ObjPrey)
            PredatorsPopulationSize = GetPredatorsPopulationSize (ObjPredators)

            print*, NbrSteps, PreyPopulationSize, PredatorsPopulationSize
!$OMP PARALLEL
!$OMP SECTIONS
!$OMP SECTION 
            NewObjPrey      => ModifyPreyPopulation      (ObjPrey,      PredatorsPopulationSize, DT)
            call PreyGarbageCollector                    (ObjPrey)

!$OMP SECTION
            NewObjPredators => ModifyPredatorsPopulation (ObjPredators, PreyPopulationSize,      DT)
            call PredatorsGarbageCollector               (ObjPredators)
!$OMP END SECTIONS NOWAIT
!$OMP END PARALLEL
        
            call Loop (NewObjPrey, NewObjPredators, DT, NbrSteps - DT)
        end if cd1
    
    end subroutine Loop
    
    !--------------------------------------------------------------------------

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



    subroutine KillLotkaVolterra (ObjLotkaVolterra)
        !Arguments-------------------------------------------------------------
        type(T_LV), pointer :: ObjLotkaVolterra

        !----------------------------------------------------------------------

        call KillPrey     (ObjLotkaVolterra%ObjPrey)
        call KillPredators(ObjLotkaVolterra%ObjPredators)

        deallocate (ObjLotkaVolterra)

    end subroutine KillLotkaVolterra
    
    !--------------------------------------------------------------------------

end program MohidLotkaVolterra
