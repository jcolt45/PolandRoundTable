#' Find organizational overlaps between two individuals
#'
#' @param member_1 An ID for a Round Table Member
#' @param member_2 An ID for a Round Table Member
#'
#' @return A tibble with all organizational overlaps and the date range of the overlap
#'
#' @import dplyr
#'
#' @export

get_shared_orgs <- function(member_1, member_2)  {

  member_1_inst <- affiliation_dates %>%
    filter(Member.ID == member_1)

  member_2_inst <- affiliation_dates %>%
    filter(Member.ID == member_2)

  all_insts <- member_1_inst %>%
    inner_join(member_2_inst,
               by = "Org.ID",
               suffix = c(".1", ".2")) %>%
    filter(Start.Date.1 < End.Date.1 &
             End.Date.2 > Start.Date.2)

  if (nrow(all_insts) > 0) {

    all_insts <- all_insts %>%
      mutate(
        Start.Date = max(Start.Date.1, Start.Date.2),
        End.Date = min(End.Date.1, End.Date.2),
      ) %>%
      select(Member.ID.1, Member.ID.2, Org.ID, Start.Date, End.Date)

  } else {

    all_insts <- all_insts %>%
      mutate(
        Member.ID.1 = member_1,
        Member.ID.2 = member_2,
        Org.ID = NA,
        Start.Date = NA,
        End.Date = NA
      ) %>%
      select(Member.ID.1, Member.ID.2, Org.ID, Start.Date, End.Date)

  }

  return(all_insts)

}


#' Find membership overlaps between two organizations
#'
#' @param org_1 An ID for an organization
#' @param org_2 An ID for an organization
#'
#' @return A tibble with all member overlaps and the date range of the overlap
#'
#' @import dplyr
#'
#' @export

get_shared_members <- function(org_1, org_2)  {

  org_1_mems <- affiliation_dates %>%
    filter(Org.ID == org_1)

  org_2_mems <- affiliation_dates %>%
    filter(Org.ID == org_2)

  all_mems <- org_1_mems %>%
    inner_join(org_2_mems,
               by = "Member.ID",
               suffix = c(".1", ".2")) %>%
    filter(Start.Date.1 < End.Date.1 &
             End.Date.2 > Start.Date.2)

  if (nrow(all_mems) > 0) {

    all_mems <- all_mems %>%
      mutate(
        Start.Date = max(Start.Date.1, Start.Date.2),
        End.Date = min(End.Date.1, End.Date.2),
      ) %>%
      select(Org.ID.1, Org.ID.2, Member.ID, Start.Date, End.Date)

  } else {

    all_mems <- all_mems %>%
      mutate(
        Org.ID.1 = org_1,
        Org.ID.2 = org_2,
        Member.ID = NA,
        Start.Date = NA,
        End.Date = NA
      ) %>%
      select(Org.ID.1, Org.ID.2, Member.ID, Start.Date, End.Date)

  }

  return(all_mems)

}

