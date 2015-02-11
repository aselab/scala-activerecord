package controllers

import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

import models._
import views.html.{user => view}
import com.github.aselab.activerecord.dsl._

object Users extends Controller {

  def index = Action {
    Ok(view.index(User.all.toList))
  }

  def show(id: Long) = Action {
    User.find(id) match {
      case Some(user) => Ok(view.show(user))
      case _ => NotFound
    }
  }

  def newPage = Action { implicit request =>
    Ok(view.edit(User.form, routes.Users.create, "Create", "User create"))
  }

  def create = Action { implicit request =>
    User.form.bindFromRequest.fold(
      errors => BadRequest(view.edit(errors, routes.Users.create, "Create", "User create")), {
      user =>
        User.transaction { user.save }
        Redirect(routes.Users.show(user.id))
    })
  }

  def edit(id: Long) = Action { implicit request =>
    User.find(id) match {
      case Some(user) => Ok(view.edit(User.form(user), routes.Users.update(id), "Update", "User edit"))
      case _ => NotFound
    }
  }

  def update(id: Long) = Action { implicit request =>
    User.find(id) match {
      case Some(user) =>
        User.form(user).bindFromRequest.fold(
          errors => BadRequest(view.edit(errors, routes.Users.update(id), "Update", "User edit")), {
          user =>
            User.transaction { user.save }
            Redirect(routes.Users.index)
        })
      case _ => NotFound
    }
  }

  def delete(id: Long) = Action {
    User.find(id) match {
      case Some(user) =>
        User.transaction { user.delete }
        Ok
      case _ => NotFound
    }
  }
}
