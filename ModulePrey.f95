!------------------------------------------------------------------------------
!        IST/MARETEC, Water Modelling Group, Mohid modelling system
!------------------------------------------------------------------------------
!
! TITLE         : Lotka Volterra
! DATE          : July 2013
! REVISION      : Ricardo Miranda
! DESCRIPTION   : Example program of Functional Core, Imperative Shell according to
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
 
Module ModulePrey

    implicit none

    !Types---------------------------------------------------------------------
    
    public :: T_Prey
    type      T_Prey
        private
        real(8), pointer :: PopulationSize
        !Population is computed every time step
        
        real(8), pointer :: IncreaseRate
        real(8), pointer :: DecreaseRate
    end type  T_Prey

    !--------------------------------------------------------------------------

    private 

    !Subroutines---------------------------------------------------------------

    !Constructor
    public  :: ConstructPrey
    private ::      AllocateInstance
    private ::      InitializeValues
    private :: AllocateReplica

    !Selector
    public  :: GetPreyPopulationSize
    public  :: GetPreyIncreaseRate
    public  :: GetPreyDecreaseRate
                         
    !Modifier
    public  :: ModifyPreyPopulation
    private ::      Birth
    private ::      Destroy

    !Destructor
    public  :: KillPrey                                                    
    private ::      DeAllocateInstance
    public  :: PreyGarbageCollector
    
    !Interfaces----------------------------------------------------------------

    !--------------------------------------------------------------------------
    
    contains


    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONS

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    function ConstructPrey(IniPopulation, IncreaseRate, DecreaseRate) 

        !Arguments---------------------------------------------------------------
        real(8), intent(IN)                             :: IniPopulation
        real(8), intent(IN)                             :: IncreaseRate
        real(8), intent(IN)                             :: DecreaseRate
        
        !Return------------------------------------------------------------------
        type (T_Prey), pointer                          :: ConstructPrey

        !Local-------------------------------------------------------------------
        type (T_Prey), pointer                          :: NewObjPrey

        !------------------------------------------------------------------------

        call AllocateInstance(NewObjPrey)
        call InitializeValues(NewObjPrey, IniPopulation, IncreaseRate, DecreaseRate)

        ConstructPrey => NewObjPrey

        !----------------------------------------------------------------------

    end function ConstructPrey
 
    !--------------------------------------------------------------------------
    
    subroutine AllocateInstance(NewObjPrey)

        !Arguments-------------------------------------------------------------
        type (T_Prey), pointer                          :: NewObjPrey
                                                    
        !Local-----------------------------------------------------------------


        !Allocates new instance
        allocate (NewObjPrey)
        allocate (NewObjPrey%PopulationSize)
        allocate (NewObjPrey%IncreaseRate)
        allocate (NewObjPrey%DecreaseRate)

    end subroutine AllocateInstance
 
    !--------------------------------------------------------------------------
    
    subroutine InitializeValues(NewObjPrey, IniPopulation, IncreaseRate, DecreaseRate)

        !Arguments-------------------------------------------------------------
        type (T_Prey), pointer                     :: NewObjPrey
        real(8), intent(IN)                             :: IniPopulation
        real(8), intent(IN)                             :: IncreaseRate
        real(8), intent(IN)                             :: DecreaseRate
                                                    
        !Local-----------------------------------------------------------------

        NewObjPrey%PopulationSize = IniPopulation
        NewObjPrey%IncreaseRate   = IncreaseRate
        NewObjPrey%DecreaseRate   = DecreaseRate

    end subroutine InitializeValues

    !--------------------------------------------------------------------------
    
    function AllocateReplica(ObjPrey) 


        !Arguments-------------------------------------------------------------
        type (T_Prey), pointer                               :: ObjPrey
                                                    
        !Return----------------------------------------------------------------
        type (T_Prey), pointer                               :: AllocateReplica

        !Local-----------------------------------------------------------------
        type (T_Prey), pointer                               :: NewObjPrey


        !Allocates new values
        allocate (NewObjPrey)
        allocate (NewObjPrey%PopulationSize)
        
        NewObjPrey%PopulationSize = -99999.9
        !New value

        NewObjPrey%IncreaseRate   => ObjPrey%IncreaseRate
        NewObjPrey%DecreaseRate   => ObjPrey%DecreaseRate

        AllocateReplica => NewObjPrey

    end function AllocateReplica


    !--------------------------------------------------------------------------


    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !SELECTOR SELECTOR SELECTOR SELECTOR SELECTOR SELECTOR SELECTOR SELECTOR SE

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    

    !--------------------------------------------------------------------------
    
    function GetPreyPopulationSize (ObjPrey)

        !Arguments-------------------------------------------------------------
        type (T_Prey), pointer                          :: ObjPrey

        !Return----------------------------------------------------------------
        real(8)                                         :: GetPreyPopulationSize

        !----------------------------------------------------------------------

        GetPreyPopulationSize = ObjPrey%PopulationSize

    end function GetPreyPopulationSize
    
    !--------------------------------------------------------------------------
    
    function GetPreyIncreaseRate (ObjPrey)

        !Arguments-------------------------------------------------------------
        type (T_Prey), pointer                          :: ObjPrey

        !Return----------------------------------------------------------------
        real(8)                                              :: GetPreyIncreaseRate

        !----------------------------------------------------------------------

        GetPreyIncreaseRate = ObjPrey%IncreaseRate

    end function GetPreyIncreaseRate
    
    !--------------------------------------------------------------------------
    
    function GetPreyDecreaseRate (ObjPrey)

        !Arguments-------------------------------------------------------------
        type (T_Prey), pointer                          :: ObjPrey

        !Return------------------------------------------------------------------
        real(8)                                              :: GetPreyDecreaseRate

        !----------------------------------------------------------------------

        GetPreyDecreaseRate = ObjPrey%DecreaseRate

    end function GetPreyDecreaseRate
    
    !--------------------------------------------------------------------------
    
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODI

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    function ModifyPreyPopulation(ObjPrey, PredatorsPopulationSize, DT)

        !Arguments-------------------------------------------------------------
        type (T_Prey),  pointer                   :: ObjPrey
        real(8), intent(IN)                       :: PredatorsPopulationSize
        real(8), intent(IN)                       :: DT
                                                    
        !Return------------------------------------------------------------------
        type (T_Prey), pointer                    :: ModifyPreyPopulation

        !Local-------------------------------------------------------------------
        type (T_Prey), pointer                    :: NewObjPrey

        !----------------------------------------------------------------------

        NewObjPrey => AllocateReplica(ObjPrey)

        NewObjPrey%PopulationSize = ObjPrey%PopulationSize                                            &
                                  + Birth  (ObjPrey%PopulationSize, ObjPrey%IncreaseRate, DT)         &
                                  + Destroy(ObjPrey%PopulationSize, ObjPrey%DecreaseRate, PredatorsPopulationSize, DT)

        ModifyPreyPopulation => NewObjPrey

    end function ModifyPreyPopulation

    !--------------------------------------------------------------------------

    function Birth(PreyPopulationSize, IncreaseRate, DT)
        !Arguments-------------------------------------------------------------
        real(8), pointer, intent(IN)                   :: PreyPopulationSize
        real(8), intent(IN)                            :: IncreaseRate
        real(8), intent(IN)                            :: DT
                                                    
        !Return----------------------------------------------------------------
        real(8)                                        :: Birth

        !Local-----------------------------------------------------------------

        !----------------------------------------------------------------------

        Birth = PreyPopulationSize * IncreaseRate * DT

        !----------------------------------------------------------------------

    end function Birth

    !--------------------------------------------------------------------------

    function Destroy(PreyPopulationSize, DecreaseRate, PredatorsPopulationSize, DT)
        !Arguments-------------------------------------------------------------
        real(8), pointer, intent(IN)                   :: PreyPopulationSize
        real(8), intent(IN)                            :: DecreaseRate
        real(8), intent(IN)                            :: PredatorsPopulationSize
        real(8), intent(IN)                            :: DT
                                                    
        !Return----------------------------------------------------------------
        real(8)                                        :: Destroy

        !Local-----------------------------------------------------------------

        Destroy =-1.0 * PreyPopulationSize * DecreaseRate * PredatorsPopulationSize * DT

        !----------------------------------------------------------------------

    end function Destroy

    !--------------------------------------------------------------------------


    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



    subroutine KillPrey(ObjPrey)

        !Arguments---------------------------------------------------------------
        type (T_Prey),     pointer                      :: ObjPrey

        !Return------------------------------------------------------------------         

        !External----------------------------------------------------------------

        !Local-------------------------------------------------------------------

        !------------------------------------------------------------------------

        !Deallocates Instance
        call DeallocateInstance (ObjPrey)

        !------------------------------------------------------------------------

    end subroutine KillPrey
        

    !------------------------------------------------------------------------
    
    
    subroutine DeallocateInstance (ObjPrey)

        !Arguments-------------------------------------------------------------
        type (T_Prey), pointer                          :: ObjPrey
                                                    
        !Local-----------------------------------------------------------------

        !------------------------------------------------------------------------

        !Deallocates instance
        deallocate (ObjPrey%PopulationSize)
        deallocate (ObjPrey%IncreaseRate)
        deallocate (ObjPrey%DecreaseRate)
        deallocate (ObjPrey)
            
    end subroutine DeallocateInstance

    !--------------------------------------------------------------------------

    subroutine PreyGarbageCollector (ObjPrey)

        !Arguments-------------------------------------------------------------
        type (T_Prey), pointer                          :: ObjPrey
                                                    
        !Local-----------------------------------------------------------------

        !------------------------------------------------------------------------

        !Partially deallocates instance
        deallocate (ObjPrey%PopulationSize)
        deallocate (ObjPrey)
            
    end subroutine PreyGarbageCollector

    !--------------------------------------------------------------------------

end module ModulePrey








