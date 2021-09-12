//
//  Actions.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/11/21.
//

import UIKit
import Foundation

extension UIViewController {
    func loadUser(completion: ((CreateUserResponse) -> Void)?) {
        let spinner = startLoading()
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v1/user")!)
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            self.defaultWithCompletion(data: data, resp: resp, error: error, completion: {
                do {
                    let output = try JSONDecoder().decode(CreateUserResponse.self, from: data!)
                    completion?(output)
                } catch {
                    self.defaultOnError(error)
                }
            })
        })
        task.resume()
    }
    
    func loadExport(completion: (() -> Void)?) {
        let spinner = startLoading()
        guard let state = User.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v2/user/" + String(state.userId) + "/export")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "GET"
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            self.defaultWithCompletion(data: data, resp: resp, error: error, completion: {
                do {
                    let output = try JSONDecoder().decode(ExportResponse.self, from: data!)
                    self.overwrite(export: output)
                    User.setDidExport()
                    completion?()
                } catch {
                    self.defaultOnError(error)
                }
            })
        })
        task.resume()
    }
    
    func addBlob(content: String, completion: (() -> Void)?) {
        let spinner = startLoading()
        guard let state = User.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v2/user/" + String(state.userId) + "/blob")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(ParseBlobRequest(content: content))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            let onUnsuccessfulStatus = { (resp: URLResponse?) -> Void in self.alertUnsuccessful("Couldn't parse items. Please use one line per item, leading with the quantity.")
            }
            let onSuccess = {
                do {
                    let output = try JSONDecoder().decode(ParseBlobResponse.self, from: data!)
                    self.insertGroceries(ingredients: output.ingredients)
                    completion?()
                } catch {
                    self.defaultOnError(error)
                }
            }
            self.withCompletion(data: data, resp: resp, error: error, completion: onSuccess, onUnsuccessfulStatus: onUnsuccessfulStatus, onError: self.defaultOnError)
        })
        task.resume()
    }
    
    func addBlob(content: String, name: String, link: String?, completion: (() -> Void)?) {
        let spinner = startLoading()
        guard let state = User.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v2/user/" + String(state.userId) + "/blob")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(ParseBlobRequest(content: content))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            let onUnsuccessfulStatus = { (resp: URLResponse?) -> Void in self.alertUnsuccessful("Couldn't parse items. Please use one line per item, leading with the quantity.")
            }
            let onSuccess = {
                do {
                    let output = try JSONDecoder().decode(ParseBlobResponse.self, from: data!)
                    var ingredients = [UUID:ReadableIngredient]()
                    for ingredient in output.ingredients {
                        ingredients[UUID()] = ingredient
                    }
                    let recipe = ReadableRecipe(name: name, link: link, active: true, rating: 0, notes: "", ingredients: ingredients)
                    self.insertRecipe(recipe: recipe)
                } catch {
                    self.defaultOnError(error)
                }
            }
            self.withCompletion(data: data, resp: resp, error: error, completion: onSuccess, onUnsuccessfulStatus: onUnsuccessfulStatus, onError: self.defaultOnError)
        })
        task.resume()
    }
    
    func addLink(link: String, active: Bool, completion: (() -> Void)?) {
        let spinner = startLoading()
        guard let state = User.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v2/user/" + String(state.userId) + "/link")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(ParseLinkRequest(link: link))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            let onUnsuccessfulStatus = { (resp: URLResponse?) -> Void in self.alertUnsuccessful("Unable to import recipe. Please enter ingredients manually.")
            }
            let onSuccess = {
                do {
                    let output = try JSONDecoder().decode(ParseLinkResponse.self, from: data!)
                    var ingredients = [UUID:ReadableIngredient]()
                    for ingredient in output.ingredients {
                        ingredients[UUID()] = ingredient
                    }
                    let recipe = ReadableRecipe(name: output.name, link: link, active: active, rating: 0, notes: "", ingredients: ingredients)
                    self.insertRecipe(recipe: recipe)
                    completion?()
                } catch {
                    self.defaultOnError(error)
                }
            }
            self.withCompletion(data: data, resp: resp, error: error, completion: onSuccess, onUnsuccessfulStatus: onUnsuccessfulStatus, onError: self.defaultOnError)
        })
        task.resume()
    }
}
